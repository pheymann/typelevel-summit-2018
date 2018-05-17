# How to derive a web client from a type
In this post you will learn how to leverage type-level computations in Scala to derive web clients from types as it is done by [Servant](https://github.com/servant/servant) in Haskell. Of course, after reading this piece you can go on and implement it on your own, but you are free to use [typedapi](https://github.com/pheymann/typedapi) which already comes with a nice set of features.

Before you dive into this post be warned that you will see a lot, and I mean a lot, of typeclasses. So make sure you know the concept.

## Preface
In my company we have a concept called _Hackweek_, which is a full week to work on small projects and ideas indepent of your daily tasks. As it happened, my colleague David and I tried to build a recommender for Github projects in Haskell. Why? Because we wanted to build something in Haskell.

During this time David introduced [Servant](https://github.com/servant/servant), a library to derive web client and server functions from types. I played with it for a while and was hooked. It makes client and server implementation really declarative, reduces the boilerplate and it adds extra typesafety. Furthermore, you can use the type of your web api as a contract between server and clients. I was hooked and started an "extensive 10 minute" Google search to find an implementation in Scala. Nothing promising showed up so I put a new point on my agenda, build something similar in Scala. And this is also the start of our journey.

## Derive a web client from a type. How hard can it be?
Short answer: it takes some nights, a lot of cursing and persistence to make it work. But lets start at the beginning. First of all, we need a small example. Lets consider we have the following api:

```
GET /users/:name?sortBy=[age] -> List[User]
```

It only consists of a single endpoint which returns a list of `Users`:

```Scala
final case class User(name: String, age: Int)
```

with a given `name: String`. Furthermore, you can sort them by their `age: Int`. What we want to end up with is a function which is derived from a typelevel represenation of our endpoint:

```Scala
(name: String, filter: Int) => F[List[User]]
```

### Represent the api as a type
First question: how do you represent the above api as a type in Scala? Lets divide and conquer. We separate the api into its different building blocks and try to find type-level representations for them. After that we merge them together.

When we take a closer look we see that our api consists of:
  * path elements identifying an endpoint: `/users`
  * segments: `:name`
  * queries: `sortBy=[age]`
  * and a method `GET` to ge a unique identifier together with the path

Or in other words, just a plain HTTP definition of a web endpoint. Now that we know what we are working with lets try and find a type-level representation.

We start with our path element. If we would work with Dotty or Typelevel-Scala we could use the a feature called literal-type:

```Scala
type Path = "users".type
```

But as we want to stay in Vanilla Scala (oh, didn't I mention that before) this isn't an option. We have to use the one tool probably every developer has to use when it comes to typelevel magic in Scala, [shapeless](https://github.com/milessabine/shapeless). It comes with a nifty class called `Witness` which generates a witness type from a given, constant instance like `String` literals or `Symbols`.


```Scala
import shapeless.Witness

val usersT = Witness("users")

type Path = usersT.T
```

But this isn't a pure type declaration, you will say. And you are right. As it is with Scala 2.12 this isn't possible. We have to go the ordinary value road first to create our types.

We now use the same technique for the segment:

```Scala
import shapeless.labelled.field

val nameT = Witness('name)

type Segment = Field[name.T, String]
```

Do you see how we included the segment's identifier in the type? This way we are not only gain information about the expected type but also what kind of value we want to see. By the way, I decided to use `Symbols` as identifiers, but you could also switch to `String` literals. 

I could go on and show you how to do it for queries, headers and so on but I think you get the gist. Now that we know how to obtain the types for our api elements we have to put them together into a single type representation. After looking though shapless features we will find `HLists` a list structure which can store elements of different types. Nice, exactly what we are looking for. 

```Scala
import shapeless.{::, HNil}

type Api = Get[List[User]] :: usersT.T :: Field[nameT.T, String] :: Field[sortByT.T, Int] :: HNil
```

Here you go. `Api` is an exact representation of the endpoint we defined at the beginning. But you don't want to write `Witnesses` and `HLists` all the time so lets wrap it up into a convinient function call:

```Scala
def api[M <: MethodElement, P <: HList, Q <: HList, Api <: HList]
      (method: M, path: PathList[P], queries: QueryList[Q])
      (implicit prepQP: Prepend.Aux[Q, P, Api]): ApiTypeCarrier[M :: Api] = ApiTypeCarrier()
      
val Api = api(Get[List[User]], Root / "users" / Segment[String]('name), Queries.add(Query[Int]('sortBy)))
```

Not clear what is happening? Lets take a look at the different elements of `def api(...)`:
  * `method` should be obvious. It takes some method instance.
  * `PathList` is a type carrier with a convinient function `def /(...)` to concatenate path elements and segments. In the end `PathList` only stores the type of an `HList` nothing more.
  
```Scala
final case class PathList[P <: HList]() {
  
  def /[S](path: Witness.Lt[S]): PathList[S :: P] = PathList()
  ...
}

val Root = PathList[HNil]()
```
  * Same is true for `QueryList`.
  * The last step is to merge all these `HLists` types into a single one. Shapeless comes again with a handy typeclass called `Prepend` which provides us with the necessary functionality. Two `HList` types go in, a single type comes out ... Type Thunderdome. And again, we use a type carrier here to work with the api type.

Whoho, we did it. We are now able to define a web api as a type. Next step is to derive an actual client function from it.

### Clients from types
So far we have a type carrier with our api:

```Scala
ApiTypeCarrier[Get[List[User]] :: QueryParam['sortBy, Int] :: SegmentParam['name.type, String] :: "users".type :: HNil]
```

Now we need to transform that into a function call `(name: String, sortBy: Int) => F[List[User]]`. So what we need is the following:
  * the types of our expected input
  * the output type
  * the path to the endpoint we want to call

All informations are available but mixed up and we need to separate them. Usually when we work with collections and want to change their shape we do a `fold` and alas shapeless has typeclasses to fold left and right over an `HList`. But we only have a type. How do we fold that? We implement a type-level fold.

#### Type-level FoldLeft
What we want is to go from an api type `Api <: HList` to `(El <: HList, KIn <: HList, VIn <: HList, Out)` with:
  * `El` al the elements in our api: `"users".type :: SegmentInput :: QueryInput :: GetCall :: HNil`
  * `KIn` the input key type: `nameT.T :: sortByT.T :: HNil`
  * `VIn` the input value type: `String :: Int :: HNil`
  * and `Out`: `List[User]`

Here, we introduced new types `SegmentInput` and `QueryInput` which act as placeholder and indicate that our api has the following elements. This representation together with the separation of input value and key types will come handy when we construct our function.

First thing, we have to define a fold function which describes how to aggregate two types:

```Scala
trait FoldLeftFunction[In, Agg] { type Out }
```

That's it. We say what goes in and what comes out. You need some examples to get a better idea? Here yoy go:

```Scala
implicit def pathElementTransformer[S, El <: HList, KIn <: HList, VIn <: HList, Out] = 
  FoldLeftFunction[S, (El, KIn, VIn, Out)] { type Out = (S :: El, KIn, VIn, Out) }

implicit def segmentElementTransformer[S <: Symbol, A, El <: HList, KIn <: HList, VIn <: HList, Out] = 
  FoldLeftFunction[SegmentParam[S, A], (El, KIn, VIn, Out)] { type Out = (SegmentInput :: El, S :: KIn, A :: VIn, Out) }

// and so on
```

Now that we can aggregate types we need a vehicle to traverse our `HList` type and transform it on the fly by using our `FoldLeftFunction` instances. I think yet another typeclass can help us here.

```Scala
trait TypeLevelFoldLeft[H <: HList, Agg] { type Out }

implicit def returnCase[Agg] = new TypeLevelFoldLeft[HNil, Agg] {
  type Out = Agg
}

implicit def foldCase[H, T <: HList, Agg, FfOut, FOut](implicit f: FoldLeftFunction.Aux[H, Agg, FfOut], 
                                                                next: Lazy[TypeLevelFoldLeft.Aux[T, FfOut, FOut]]) = 
  new TypeLevelFoldLeft[H :: T, Agg] { type Out = FOut }
```

The above definition describes a recursive function which will apply the `FoldLeftFunction` on `H` and the current aggregated type `Agg` and continues with the result `FfOut` and the remaining list. And before you bang your head against the wall for hours until the clock reaches 3am like I did a small hint, make `next` lazy. Otherwise, Scala is not able to find `next`. My guess is that Scala is not able to infer `next`, because it depends on `FfOut` which is also unknown. So we have to defer `next`'s inference to give the compiler some time to work.

#### Collect all the request data
We folded our api type into the new representation. Now we use that to build a function which collects all the data necessary to make a request.

```Scala
type Uri     = List[String]
type Queries = Map[String, List[String]]

VIn => (Uri, Queries)
```

By now, you should be already comfortable with typeclasses. Therefore, it shouldn't shock you that I will introduce yet another one.

```Scala
trait RequestDataBuilder[El <: HList, KIn <: HList, VIn <: HList, O] {

  type Out <: HList

  def apply(inputs: VIn, uri: Builder[String, Uri], queries: Queries): (Uri, Queries)
}
```

Instances of this typeclass update `uri` and `queries` depending on the types they see. For example, if the current head of `El` is a path element we append its `String` literal to `uri`:

```Scala
implicit def pathBuilder[P, T <: HList, KIn <: HList, VIn <: HList, O](implicit wit: Witness.Aux[P], next: RequestDataBuilder[T, KIn, VIn, O]) = 
  new RequestDataBuilder[P :: T, KIn, VIn, O] {
    type Out = next.Out

    def apply(inputs: VIn, uri: Builder[String, List[String]], queries: Map[String, List[String]], headers: Map[String, String]): Out =
      next(inputs, uri += wit.value.toString(), queries, headers)
  }
```

Or if we encounter a query input we derive the key type literal, pair it with the given input value and add both to `queries`:

```Scala
implicit def queryBuilder[K <: Symbol, V, T <: HList, KIn <: HList, VIn <: HList, O](implicit wit: Witness.Aux[K], next: RequestDataBuilder[T, KIn, VIn, O]) = 
  new RequestDataBuilder[QueryInput :: T, K :: KIn, V :: VIn, O] {
    type Out = next.Out

    def apply(inputs: V :: VIn, uri: Builder[String, List[String]], queries: Map[String, List[String]], headers: Map[String, String]): Out =
      next(inputs.tail, uri, Map(wit.value.name -> List(inputs.head.toString())) ++ queries)
  }
```

The instances for segments or queries for `List` inputs and all the other cases are looking quite similar. So it is up to you to figure them out. 

In the end, this function should produce the following output for our example:

```Scala
// name = joe and sortBy = 42
(List("users", "joe"), Map("sortBy" -> List("42")))
```

#### Make the request
We have all the data we need to make an IO request. The thing that is keeping us from actually doing is that we are missing a HTTP backend. We have to connect the pure desription of a request with the realworld execution. And this connection should be generic, which means independent of some vendor implementation like akka-http or http4s. 

How do we achieve that? Right, typeclasses.

```Scala
trait ApiRequest[M, D, F[_], C, Out] {

  def apply(data: D, client: C): F[Out]
}
```

We have to specialize that for the set of methods we have.

```Scala
trait GetRequest[C, F[_], Out] extends ApiRequest[GetCall, (List[String, Map[String, List[String]]]), C, F, Out] {

  def apply(data: (List[String, Map[String, List[String]]]), client: C): F[Out] = {
    val (uri, queries) = data

    apply(uri, queries, client)
  }
  
  def apply(uri: List[String], queries: Map[String, List[String]], headers: Map[String, String], client: C): F[A]
}
```

Lets say we want http4s as our backend. Then we just have to implement these `traits` using http4s functionality.

#### Make it a whole
We have a bunch of typeclasses which in theory do a request, but so far they are completely useless. To make a working piece of code out of it we have to connect them.

```Scala
def derive[Api <: HList, El <: HList, KIn <: HList, VIn <: HList, Out, D, F[_], C]
  (api: ApiTypeCarrier[Api], client: C)
  (implicit fold: Lazy[TypeLevelFoldLeft.Aux[Api, Fold], (El, KIn, VIn, Out)]
            builder: RequestBuilder.Aux[El, KIn, VIn, Out, D],
            request: ApiRequest[D, F, C, Out]): VIn => F[Out] = vin => request(builder.apply(vin, List.newBuilder, Map.empty), client)
```

This first approach gives us the desired function, but has a major drawback. You have to fix `F[_]` somehow and the only way is so far is to set it explicitly. But by doing that you are forced to provided definitions for all the type parameters. Furthermore, this functions isn't really convinient. To use it you have to create and pass a `HList` and as we said before, we don't want to expose it.

To fix the first problem we simply add a helper class which moves the step of defining the higherkind `F[_]` to a separate function call:

```Scala
final class ExecutableDerivation[El <: HList, KIn <: HList, VIn <: HList, O, D](builder: RequestDataBuilder.Aux[El, KIn, VIn, O, D], input: VIn) {

  final class Derivation[F[_]] {

    def apply[C](client: C)(implicit req: ApiRequest[D, C, F, O]): F[O] = {
      val data = builder(input, List.newBuilder, Map.empty, Map.empty)

      req(data, cm)
    }
  }

  def run[F[_]]: Derivation[F] = new Derivation[F]
}
```

Making a function of arity `Length[VIn]` out of `Vin => F[O]`is possible by using `shapeless.ops.function.FnFromProduct`.

When we apply both solution we end up with:

```Scala
def derive[H <: HList, Fold, El <: HList, KIn <: HList, VIn <: HList, Out, D <: HList]
  (apiList: ApiTypeCarrier[H])
  (implicit fold: Lazy[TypeLevelFoldLeft.Aux[H, (HNil, HNil, HNil), (El, KIn, VIn, Out)]],
            builder: RequestDataBuilder.Aux[El, KIn, VIn, Out, D],
            vinToFn: FnFromProduct[VIn => ExecutableDerivation[El, KIn, VIn, Out, D]]): vinToFn.Out = 
  vinToFn.apply(input => new ExecutableDerivation[El, KIn, VIn, Out, D](builder, input))
```

Finally, we did it! We convinced the Scala compiler to derive a client function from a type. Lets have a look at out example to see how it works.

```Scala
import cats.effect.IO
import org.http4s.client.Client

val Api = api(Get[List[User]], Root / "users" / Segment[String]('name), Queries.add(Query[Int]('sortBy)))
val get = derive(Api)

get("joe", 42).run[IO](Client[IO]) // IO[List[User]]
```

## Next level - Typedapi
Now that we are able to derive a single client function from a type we should also be able to do the same for a collection of api types. And if we are alsready on it, lets add server side support. Or ... you just use [typedapi](https://github.com/pheymann/typedapi). It comes with the following features:
 * client function derivation
 * server function derivation
 * single and multi api type handling
 * support for htt4s
 * support for akka-http in the making
 * simple interface to add more HTTP frameworks/libraries

