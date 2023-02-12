# Haskell lenses

## Motivation

Pure functional programming implies that the language
and the compiler or interpreter prevent you from assigning
to variables after you have defined them and also modifying and
mutating data structures.

Normally you would do it like this in languages like Python or Java:

```python
class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __repr__(self):
        return "Point({0}, {1})".format(self.x, self.y)

class Atom(object):
    def __init__(self, element, point):
        self.element = element
        self.point = point
    def __repr__(self):
        return "Atom({0}, {1})".format(repr(self.element), self.point)

>>> atom = Atom("Fe", Point(2.0, 1.0))
>>> atom
Atom('Fe', Point(2.0, 1.0))
```

```
>>> a.point.x = 5.0
>>> atom
Atom('Fe', Point(5.0, 1.0))
```

You get a new `Atom` with a `Point` `x` value of `5.0`. 
However, your old `atom` is destroyed. This is what is known as "destructive update".
Pure functional programming avoids this, and Haskell does not allow this at all.
What you do in Haskell is creating a whole new data structure which is just like
the old one except this one field, and you also keep your old data structure
if you want.

The corresponding Haskell definitions:

```haskell
data Atom = Atom { 
  _element :: String, 
  _point :: Point
  } deriving Show

data Point = Point {
  _x :: Double,
  _y :: Double
  } deriving Show

atom :: Atom
atom = Atom {
  _element = "Fe",
  _point = Point {
    _x = 2.0,
    _y = 1.0
    }
  }
```

Now if you want to have a new `Atom` with a new `Point` with an `x` value of `5.0`
(as described before), this or similar code is what you would come up with:

```haskell
newAtom :: Atom
newAtom = atom {
  _point = (_point atom) { _x = 5.0 }
  }
```

The result:

```haskell
Prelude> atom
Atom {_element = "Fe", _point = Point {_x = 2.0, _y = 1.0}}
Prelude> newAtom
Atom {_element = "Fe", _point = Point {_x = 5.0, _y = 1.0}}
```

This is already getting tedious for such a simple task. It will get even more
tedious if you have more than two layers in your data structure or if you need to
use your old value to compute the new one (like `atom.point.x = atom.point.x + 10.0`)
This is where lenses come in. They enable retrieving fields from deeply nested
records and other data structures and "modifying" the fields.

## Usage

### How to construct

If you are using the `lens` library and want to add lenses to your record types,
here are the two major ways to do it:

* Template Haskell and `makeLenses`

```haskell
{-# LANGUAGE TemplateHaskell #-} 
-- ^ this has to be on top of the file, where all the extensions are

import Control.Lens

data Atom = Atom { 
  _element :: String, 
  _point :: Point
  } deriving Show
makeLenses ''Atom

data Point = Point {
  _x :: Double,
  _y :: Double
  } deriving Show
makeLenses ''Point
```

In essence, here you are adding `makeLenses` statemens alongside with the definitions.
Also, no `TemplateHaskell` needed.


* The `lens` function

Instead of using `makeLenses` statement, you can construct a `Lens`
from a getter and a setter using
the [`lens` function](https://github.com/ekmett/lens/blob/a901d6e5a4b22ca5c6db6e4540f6ef14141c7895/src/Control/Lens/Lens.hs#L214-L215):

```haskell
point :: Lens' Atom Point
point = lens _point (\atom newPoint -> atom { _point = newPoint })
```

### How to get and set fields

The point of lenses is enabling you to 'zoom in' into deeper structures.
You can do this using regular function composition `(.)`:

```haskell
> :type point
point :: Functor f => (Point -> f Point) -> Atom -> f Atom
> :type x
x :: Functor f => (Double -> f Double) -> Point -> f Point
> :type (point . x)
(point . x) :: Functor f => (Double -> f Double) -> Atom -> f Atom
```

As you can see, `point . x` is also a `Lens` which will get you
a `Double` from an `Atom`.

`view` and `(^.)` functions let you obtain field values:

```haskell
> atom = Atom { _element = "Fe", _point = Point { _x = 1.0, _y = 2.0 } }
> view (point.x) atom
1.0
> atom ^. point.x
1.0
```

(It is recommended to omit spaces around the dot to distinguish
lens composition from other instances of function composition.)

`set` and `(.~)` functions let you set new values for your fields:

```haskell
> newAtom1 = atom & point.x .~ 100.0
> newAtom1
Atom {_element = "Fe", _point = Point {_x = 100.0, _y = 2.0}}
> newAtom2 = set (point.x) 100.0 atom
> newAtom2
Atom {_element = "Fe", _point = Point {_x = 100.0, _y = 2.0}}
```

`(&)` is a [version](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data-Function.html#%26)
of function application `($)` with arguments flipped,
that is, it applies the thing on its right as a function to the thing
on its left as an argument to that function.

So the pattern for using `(.~)` is as follows: `structure & ourLens .~ newValue`

Sometimes you may need to modify fields in a structure as opposed to setting
(overwriting) them (that is, use the previous field value to compute a new one).
This can be achieved with functions `over` and `(%~)`:

```haskell
> over (point.x) (+ 50) atom
Atom {_element = "Fe", _point = Point {_x = 51.0, _y = 2.0}}
> atom & point.x %~ (+ 50)
Atom {_element = "Fe", _point = Point {_x = 51.0, _y = 2.0}}
```

That should cover most of the things you need to be productive with lenses.
Keep reading if you want to dive into internals of lenses and some advanced usage.


## How does this actually work?

How on earth can a thing like `Functor f => (a -> f b) -> (s -> f t)
work as a pair of a getter and a setter?

To find out, we will look into what actually happens when getting and setting.

Let's start with getting. This is the [definition](https://github.com/ekmett/lens/blob/a901d6e5a4b22ca5c6db6e4540f6ef14141c7895/src/Control/Lens/Getter.hs#L316-L317)
of `(^.)` which looks quite obscure (I've modified the type a bit so it is more consistent
with what we've seen so far, shouldn't affect anything if you are using lenses only):

```haskell
(^.) :: s -> Lens' s a -> a
s ^. l = getConst (l Const s)
```

It is not obvious at all what is going on with this code, we will rewrite it
a bit to get more clarity, renaming `s` to `struct` and `l` to `lens`
and also expanding signatures (you will need `RankNTypes`, `ScopedTypeVariables`
and `TypeApplications` GHC extensions for this to work):

```haskell
myGet1 :: forall a s. s -> (forall f. Functor f => (a -> f a) -> s -> f s) -> a
myGet1 struct lens = getConst (lens @(Const a) (Const @a @a) struct)
```

(`Const` comes from `Data.Functor.Const`.)

The big idea with van Laarhoven lenses (those with the signature
`forall f. Functor f => (a -> f b) -> s -> f t)` that we are
talking about here) is that you plug in various particular functors
for `f` depending on what you are doing with the lens. Here we are
pligging `Const a` for `f` (that is what the type application `@(Const a)`
achieves). Thus the type of our `l` becomes

```haskell
l @(Const a) :: (a -> Const a a) -> s -> Const a s
```

If we imagine for a second that our `Const` is defined as a type synonym
rather than a `newtype` our type is then

```haskell
l :: (a -> a) -> s -> a
```

, in other words, our type is isomorphic to this type. If you are familiar
with contiuaiton-passing style this should resemble it. `lens` expects
a function telling it what to do with an `a` and then it does that to
the `a` inside `s` that `lens` is pointing to. If we want to get `a` as is
we would want to apply `lens` to `id`.

That is the idea behind getting. However, we have newtypes rather than
type synonyms, so the newtype wrapper `Const` here is acting as the `id`
function.

To make the intent absolutely clear, let's expand everything that we have:

```haskell
myGet2 :: forall a s. s -> (forall f. Functor f => (a -> f a) -> s -> f s) -> a
myGet2 struct lens =
  let myIdFunction :: a -> Const a a
      myIdFunction = Const @a @a
      wrappedFieldValue :: Const a s wrappedFieldValue = lens @(Const a) (Const @a @a) struct
                                                   -- a `Const` newtype that contains
                                                   -- a value of type `a`
  in getConst wrappedFieldValue -- returns a value of type a
```

Let's look at how the expression that performs getting is reduced:
```haskell
   Atom "Fe" (Point 1.0 2.0) ^. point
== Atom "Fe" (Point 1.0 2.0) ^. lens _point (\atom newPoint -> atom { _point = newPoint })
   -- ^ by definition of `point`
== getConst (lens _point (\atom newPoint -> atom { _point = newPoint }) Const (Atom "Fe" (Point 1.0 2.0)))
   -- ^ by definition of `(^.)`
== getConst (((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 1.0 2.0))) <$> Const (_point (Atom "Fe" (Point 1.0 2.0))))
   -- ^ by definition of `lens`
== getConst ((fmap @(Const Point) ((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 2.0 2.0)))) (Const (_point (Atom "Fe" (Point 1.0 2.0)))))
   -- ^ by definition of `(<$>)` which is an infix version of `fmap`,
   -- type application is inserted to make clear
   -- that `fmap` comes from `Const Point` functor instance
== getConst (Const (_point (Atom "Fe" (Point 1.0 2.0))))
   -- ^ by definition of `fmap` for `Const Point`
   --   https://gitlab.haskell.org/ghc/ghc/-/blob/a54827e0b48af33fa9cfde6ad131c6751c2fe321/libraries/base/Data/Functor/Const.hs#L83
== _point (Atom "Fe" (Point 1.0 2.0))
   -- ^ wrap and unwrap newtype
== Point 1.0 2.0
```

We proceed to setting. Simplifying the [definition](https://github.com/ekmett/lens/blob/a901d6e5a4b22ca5c6db6e4540f6ef14141c7895/src/Control/Lens/Setter.hs#L402-L403):

```haskell
set' :: forall a s. (forall f. Functor f => (a -> f a) -> s -> f s) -> a -> s -> s
set' lens newValue = runIdentity . lens @Identity (\_ -> Identity newValue)
```

What makes this work (other than wrapping and unwrapping newtypes)
is plugging `Identity` for our `f` functor and applying the `lens`
to a function that always returns `newValue`
(essentially `const newValue`). The function we get after this
(of type `s -> s`) is meant to apply that function to a field value of
the input structure `s` and return the resulting `s`.
In other words, you tell it what to do with the `a`
(`a -> Identity a`)
and it lets you do it inside an `s`
(`s -> Identity s`).

Now we look at how an example setting is evaluated:

```haskell
   set' point (Point 100.0 100.0) (Atom "Fe" (Point 1.0 2.0))
== (runIdentity . point @Identity (\_ -> Identity (Point 100.0 100.0))) (Atom "Fe" (Point 1.0 2.0))
   -- ^ by definition of `set'`
== (runIdentity . (lens _point (\atom newPoint -> atom { _point = newPoint }) (\_ -> Identity (Point 100.0 100.0)))) (Atom "Fe" (Point 1.0 2.0))
   -- ^ by definition of our `point` lens
== (runIdentity . (\s -> (\atom newPoint -> atom { _point = newPoint }) s <$> (\_ -> Identity (Point 100.0 100.0)) (_point s))) (Atom "Fe" (Point 1.0 2.0))
   -- ^ by definition of `lens`
== (\s -> runIdentity ((\atom newPoint -> atom { _point = newPoint }) s <$> (\_ -> Identity (Point 100.0 100.0)) (_point s))) (Atom "Fe" (Point 1.0 2.0))
   -- ^ by definition of Haskell function composition `(.)`
== runIdentity ((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 1.0 2.0)) <$> (\_ -> Identity (Point 100.0 100.0)) (_point (Atom "Fe" (Point 1.0 2.0))))
   -- ^ performing function application:
   --   everywhere in the function body
   --   `s` gets replaced with `Atom "Fe" (Point 1.0 2.0)`
== runIdentity ((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 1.0 2.0)) <$> (\_ -> Identity (Point 100.0 100.0)) (Point 1.0 2.0))
   -- ^ by definition of `_point` field selector
== runIdentity ((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 1.0 2.0)) <$> Identity (Point 100.0 100.0))
   -- ^ performing function application
== runIdentity (Identity ((\atom newPoint -> atom { _point = newPoint }) (Atom "Fe" (Point 1.0 2.0)) (Point 100.0 100.0)))
   -- ^ by definition of `(<$>)` (`fmap` of `Identity`)
   --   https://gitlab.haskell.org/ghc/ghc/-/blob/a54827e0b48af33fa9cfde6ad131c6751c2fe321/libraries/base/Data/Functor/Identity.hs#L119
== runIdentity (Identity (Atom "Fe" (Point 100.0 100.0)))
   -- ^ performing function application
== Atom "Fe" (Point 100.0 100.0)
   -- ^ unwrapping the newtype
```

It might be instructive to look at how `over` works too.
It is defined in the library as just `coerce`:

```haskell
over :: ASetter s t a b -> (a -> b) -> s -> t
over = coerce
```

It takes a function of type `a -> b` rather than a constant `b` value.
and all that `coerce` does is wrapping and unwrapping newtypes.
If we wanted to do it like with `set'` and handle newtypes
manually we would write:

```haskell
over :: forall a b s t. (forall f. (a -> f b) -> s -> f t) -> (a -> b) -> s -> t
over lens modifier = runIdentity . lens @Identity (Identity . modifier)
```

We also could have implemented `set` and `set'` in terms of `over`:

```haskell
set :: forall a b s t. (forall f. (a -> f b) -> s -> f t) -> b -> s -> t
set lens newValue = over lens (\_ -> newValue)
```

## Further reading

If you are looking for more thorough explanations of lenses
you might be interested in these papers:

- https://sinusoid.es/misc/lager/lenses.pdf (lens usage in more detail)
- https://arxiv.org/pdf/1809.00738.pdf
- https://arxiv.org/pdf/1103.2841.pdf (in particular sections 2 and 4)
