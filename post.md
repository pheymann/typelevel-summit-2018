# Typedapi or how to derive your clients and servers from types
I am fantastically bad at learning new programming concepts just by reading books and doing the exercises. What I usually do is try to solve a problem I am interested in and learn on-the-fly. And with interesting I mean it should be challenging and it should solve a pain that at least I have. The thing is, coming up with a project idea which fulfills these criteria is quite hard. But as it often is with thinks you are looking for but cannot find, at the time you stop searching they just reveal themselves.

This was also the case when I started to take a deep dive into Scala's type-level computation realm. I was looking into this concept for a while when a colleague of mine introduced [Servant](https://github.com/servant/servant) while we were spending some time on a toy-project. For everyone not knowing it, Servant is a Haskell library which lets you define your web apis as types and derives the client and server functions from it. I saw it and loved the idea. Creating web server and clients this way reduces your code to a mere type, you get extra type safety and you can use the api types as contracts between your server and its clients. After a while playing with Servant I realized that it would make a perfect candidate for learning how to do computations with types in Scala.

But I just wanted to start with a single feature to not overwhelm myself and abandoned the project after a couple of hours. Therefore, I set out to make Scala able to derive a client function from a single api type and this will also be the topic of the following blog post.

## Derive a client function from a type. How hard can it be?
Short answer: it takes some nights and a lot of cursing. But let's start at the beginning. First of all, I will introduce a small example we will use to ease the understanding later on. Consider the following api:

```
GET /users/:name?minAge=[age] -> List[User]
```

It only consists of a single endpoint which returns a list of `Users`:

```Scala
final case class User(name: String, age: Int)
```

with a given `name: String`. Furthermore, you filter the resulting users by their `age: Int`. Our big goal is to end up with a function which is derived from a type-level representation of our endpoint:

```Scala
(name: String, minAge: Int) => F[List[User]]
```

### Represent the api as a type
Next question: how do you represent the above api as a type in Scala? I think we can divide and conquer here. We separate the api into its different building blocks and try to find type-level representations for each of them. After that, we merge all the stuff together.

When we take a closer look at our endpoint we see that it consists of:
  * path elements identifying an endpoint: `/users`
  * segments within the path: `:name`
  * queries: `minAge=[age]`
  * and a method `GET` to identify which kind of operation we want to do

Or in other words, just a plain HTTP definition of a web endpoint. Now that we know what we are working with let's try and find a type-level representation.

We start with our path element. If we would work with Dotty or Typelevel-Scala we could use a feature called literal-type:

```Scala
type Path = "users"
```

But as we want to stay in Vanilla Scala (oh, didn't I mention that before) this isn't an option. We have to use the one tool probably every developer has to use when it comes to working on the type-level in Scala called [shapeless](https://github.com/milessabine/shapeless). It comes with a nifty class called [Witness](https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/singletons.scala#L31) which generates a witness type from a constant value like `Strings` or `Symbols`.

```Scala
import shapeless.Witness

val usersW = Witness("users")
```

But this isn't a pure type declaration, you will say. And you are right, but right now there is no other way in Scala. We have to go the ordinary value road first to create our types.

Now that we know how to get a type representation from a `String` which describes our path we should clearly mark it as a path element:

```Scala
sealed trait Path[P]

type users = Path[usersW.T]
```

That's it. That is the basic concept of how we can describe our apis as types. We just reuse this concept now for the remaining element like the segment.


```Scala
val nameW = Witness('name)

sealed trait Segment[K, V]

type name = Segment[nameW.T, String]
```

Do you see how we included the segment's identifier in the type? This way we are not only gain information about the expected type but also what kind of value we want to see. By the way, I decided to use `Symbols` as identifiers, but you could also switch to `String` literals. The remaining definitions look pretty similar:

```Scala
val minAgeW = Witness('minAge)

sealed trait Query[K, V]

type minAge = Query[minAgeW.T, Int]

sealed trait Method
sealed trait Get[A] extends Method
```

Now that we know how to obtain the types of our api elements we have to put them together into a single type representation. After looking through shapeless's features we will find `HLists`, a list structure which can store elements of different types.

```Scala
import shapeless.{::, HNil}

type Api = Get[List[User]] :: users :: name :: minAge :: HNil
```

Here you go. `Api` is an exact representation of the endpoint we defined at the beginning. But you don't want to write `Witness` and `HLists` all the time so let's wrap it up into a convenient function call:

```Scala
def api[M <: Method, P <: HList, Q <: HList, Api <: HList]
       (method: M, path: PathList[P], queries: QueryList[Q])
       (implicit prepQP: Prepend.Aux[Q, P, Api]): ApiTypeCarrier[M :: Api] = ApiTypeCarrier()
      
val Api = api(Get[List[User]], Root / "users" / Segment[String]('name), Queries.add(Query[Int]('minAge)))
```

Not clear what is happening? Let's take a look at the different elements of `def api(...)`:
  * `method` should be obvious. It takes some method type.
  * `PathList` is a type carrier with a function `def /(...)` to concatenate path elements and segments. In the end, `PathList` only stores the type of an `HList` and nothing more.
  
```Scala
final case class PathList[P <: HList]() {
  
  def /[S](path: Witness.Lt[S]): PathList[S :: P] = PathList()
  ...
}

val Root = PathList[HNil]()
```
  * Same is true for `QueryList`.
  * The last step is to merge all these `HLists` types into a single one. Shapeless comes again with a handy type class called `Prepend` which provides us with the necessary functionality. Two `HList` types go in, a single type comes out. And again, we use a type carrier here to store the api type.

Whoho, we did it. One thing we can check on our todo list. Next step is to derive an actual client function from it.

### Clients from types
So far we have a type carrier with our api:

```Scala
ApiTypeCarrier[Get[List[User]] :: Query[minAgeW.T, Int] :: Segment[nameW.T, String] :: usersW.T :: HNil]
```

Now we need to transform that into a function call `(name: String, minAge: Int) => F[List[User]]`. So what we need is the following:
  * the types of our expected input
  * the output type
  * the path to the endpoint we want to call

All information are available but mixed up and we need to separate them. Usually, when we work with collections and want to change their shape we do a `fold` and alas shapeless has type classes to fold left and right over an `HList`. But we only have a type. How do we fold that?

#### Type-level FoldLeft
What we want is to go from `Api <: HList` to `(El <: HList, KIn <: HList, VIn <: HList, M, Out)` with:
  * `El` al the elements in our api: `"users".type :: SegmentInput :: QueryInput :: GetCall :: HNil`
  * `KIn` the input key types: `nameW.T :: minAgeW.T :: HNil`
  * `VIn` the input value types: `String :: Int :: HNil`
  * the method type: `GetCall`
  * and `Out`: `List[User]`

Here, we introduced new types `SegmentInput` and `QueryInput` which act as placeholders and indicate that our api has the following inputs. This representation will come in handy when we construct our function.

Now, how to fold on the type-level? The first step, we have to define a function which describes how to aggregate two types:

```Scala
trait FoldLeftFunction[In, Agg] { type Out }
```

That's it. We say what goes in and what comes out. You need some examples to get a better idea? Here you go:

```Scala
implicit def pathTransformer[P, El <: HList, KIn <: HList, VIn <: HList, M, Out] = 
  FoldLeftFunction[Path[P], (El, KIn, VIn, M, Out)] { type Out = (P :: El, KIn, VIn, Out) }

implicit def segmentTransformer[K <: Symbol, V, El <: HList, KIn <: HList, VIn <: HList, M, Out] = 
  FoldLeftFunction[Segment[K, V], (El, KIn, VIn, M, Out)] { type Out = (SegmentInput :: El, K :: KIn, V :: VIn, Out) }

// and so on
```

Now that we can aggregate types we need a vehicle to traverse our `HList` type and transform it on the fly by using our `FoldLeftFunction` instances. I think yet another type class can help us here.

```Scala
trait TypeLevelFoldLeft[H <: HList, Agg] { type Out }

implicit def returnCase[Agg] = new TypeLevelFoldLeft[HNil, Agg] {
  type Out = Agg
}

implicit def foldCase[H, T <: HList, Agg, FfOut, FOut](implicit f: FoldLeftFunction.Aux[H, Agg, FfOut], 
                                                                next: Lazy[TypeLevelFoldLeft.Aux[T, FfOut, FOut]]) = 
  new TypeLevelFoldLeft[H :: T, Agg] { type Out = FOut }
```

The above definition describes a recursive function which will apply the `FoldLeftFunction` on `H` and the current aggregated type `Agg` and continues with the resulting `FfOut` and the remaining list. And before you bang your head against the wall for hours until the clock strikes 3 am, like I did, a small hint, make `next` lazy. Otherwise, Scala is not able to find `next`. My guess is that Scala is not able to infer `next`, because it depends on `FfOut` which is also unknown. So we have to defer `next`'s inference to give the compiler some time to work.

And another hint, you can start with `Nothing` as the initial type for your aggregate.

#### Collect all the request data
We folded our api type into the new representation. Now we use that to build a function which collects all the data necessary to make a request.

```Scala
type Uri     = List[String]
type Queries = Map[String, List[String]]

VIn => (Uri, Queries)
```

By now, you should be already comfortable with type classes. Therefore, it shouldn't shock you that I will introduce yet another one.

```Scala
trait RequestDataBuilder[El <: HList, KIn <: HList, VIn <: HList] {

  def apply(inputs: VIn, uri: Uri, queries: Queries): (Uri, Queries)
}
```

Instances of this type class update `uri` and `queries` depending on the types they see. For example, if the current head of `El` is a path element we prepend its `String` literal to `uri`. Just keep in mind to reverse the `List` before returning it.

```Scala
implicit def pathBuilder[P, T <: HList, KIn <: HList, VIn <: HList](implicit wit: Witness.Aux[P], next: RequestDataBuilder[T, KIn, VIn]) = 
  new RequestDataBuilder[P :: T, KIn, VIn] {
    def apply(inputs: VIn, uri: Uri, queries: Queries): (Uri, Queries) =
      next(inputs, wit.value.toString() :: uri, queries, headers)
  }
```

Or if we encounter a query input we derive the key's type-literal, pair it with the given input value and add both to `queries`:

```Scala
implicit def queryBuilder[K <: Symbol, V, T <: HList, KIn <: HList, VIn <: HList](implicit wit: Witness.Aux[K], next: RequestDataBuilder[T, KIn, VIn]) = 
  new RequestDataBuilder[QueryInput :: T, K :: KIn, V :: VIn] {
    def apply(inputs: V :: VIn, uri: Uri, queries: Queries): (Uri, Queries) =
      next(inputs.tail, uri, Map(wit.value.name -> List(inputs.head.toString())) ++ queries)
  }
```

The other cases are looking quite similar and it is up to the interested reader to find the implementations.

What we end up with is a nested function call structure which will take an `HList` and returns the `uri` and `queries`.

```Scala
"joe" :: 42 :: HNil => (List("users", "joe"), Map("minAge" -> List("42")))
```

#### Make the request
We have all the data we need to make an IO request but nothing to execute it. We change that now. By adding an HTTP backend. But we don't want to expose this implementation detail through our code. What we want is a generic description of a request action and that sounds again like a job for type classes.

```Scala
trait ApiRequest[M, F[_], C, Out] {

  def apply(data: (Uri, Queries), client: C): F[Out]
}
```

We have to specialize that for the set of methods we have:

```Scala
trait GetRequest[C, F[_], Out] extends ApiRequest[GetCall, C, F, Out]

...
```

Let's say we want http4s as our backend. Then we just have to implement these `traits` using http4s functionality.

#### Make it a whole
We have a bunch of type classes which in theory do a request, but so far they are completely useless. To make a working piece of code out of it we have to connect them.

```Scala
def derive[Api <: HList, El <: HList, KIn <: HList, VIn <: HList, M, Out, F[_], C]
  (api: ApiTypeCarrier[Api], client: C)
  (implicit fold: Lazy[TypeLevelFoldLeft.Aux[Api, Fold], (El, KIn, VIn, M, Out)]
            builder: RequestBuilder[El, KIn, VIn],
            request: ApiRequest[M, F, C, Out]): VIn => F[Out] = vin => request(builder.apply(vin, List.newBuilder, Map.empty), client)
```

This first approach gives us the desired function but has a major drawback. You have to fix `F[_]` somehow and the only way is so far is to set it explicitly. But by doing that you are forced to provide definitions for all the type parameters. Furthermore, this function isn't really convenient. To use it you have to create and pass an `HList` and as we said before, we don't want to expose something like that.

To fix the first problem we simply add a helper class which moves the step of defining the higher kind `F[_]` to a separate function call:

```Scala
final class ExecutableDerivation[El <: HList, KIn <: HList, VIn <: HList, M, O](builder: RequestDataBuilder[El, KIn, VIn], input: VIn) {

  final class Derivation[F[_]] {

    def apply[C](client: C)(implicit req: ApiRequest[M, C, F, O]): F[O] = {
      val data = builder(input, List.newBuilder, Map.empty, Map.empty)

      req(data, cm)
    }
  }

  def run[F[_]]: Derivation[F] = new Derivation[F]
}
```

Making a function of arity `Length[VIn]` out of `Vin => F[O]`is possible by using `shapeless.ops.function.FnFromProduct`.

When we apply both solutions we end up with:

```Scala
def derive[H <: HList, Fold, El <: HList, KIn <: HList, VIn <: HList, M, Out]
  (apiList: ApiTypeCarrier[H])
  (implicit fold: Lazy[TypeLevelFoldLeft.Aux[H, Nothing, (El, KIn, VIn, M, Out)]],
            builder: RequestDataBuilder[El, KIn, VIn],
            vinToFn: FnFromProduct[VIn => ExecutableDerivation[El, KIn, VIn, M, Out]]): vinToFn.Out = 
  vinToFn.apply(input => new ExecutableDerivation[El, KIn, VIn, M, Out](builder, input))
```

Finally, we did it! We convinced the Scala compiler to derive a client function from a type. Let's have a look at our example to see how it works.

```Scala
import cats.effect.IO
import org.http4s.client.Client

val Api = api(Get[List[User]], Root / "users" / Segment[String]('name), Queries.add(Query[Int]('minAge)))
val get = derive(Api)

get("joe", 42).run[IO](Client[IO]) // IO[List[User]]
```

## Next level - Typedapi
Now that we are able to derive a single client function from a type we should also be able to do the same for a collection of api types. And if we are already on it, let's add server-side support. Or ... you just use [typedapi](https://github.com/pheymann/typedapi). It already comes with the following features:
 * client function derivation
 * server function derivation
 * single and multi api type handling
 * support for htt4s
 * support for akka-http in the making
 * simple interface to add more HTTP frameworks/libraries
