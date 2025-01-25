package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.empty
import fpinscala.exercises.laziness.LazyList.cons

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _ => Empty
  

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  

  def takeWhile_1(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty

  def takeWhile(p: A => Boolean): LazyList[A] = 
    this.foldRight(empty) ( (a, b) => if (p(a)) cons(a, b) else empty )
  

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = 
    this.foldRight(None: Option[A])((a,_) => Option[A](a))

  def map[B](f: A => B): LazyList[B] = 
    this.foldRight(empty[B]) {(a, b) => cons(f(a), b)}

  def filter(p: A => Boolean): LazyList[A] = 
    this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] = 
    this.foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(empty){(a,b) => f(a).append(b)}

  def startsWith[B](s: LazyList[B]): Boolean = zipAll(s).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] = 
    LazyList.unfold((n, this)) {
      case (v, Cons(h, t)) if v > 0 => Some((h(), (v - 1, t())))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = 
    LazyList.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  
  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] = 
    LazyList.unfold((this, other)) {
      case (Cons(th, tt), Cons(oh, ot)) => Some((f(th(), oh()), (tt(), ot())))
      case _ => None
    }
  
  def zipAll[B](l2: LazyList[B]): LazyList[(Option[A], Option[B])] = 
    LazyList.unfold((this, l2)) {
      case (Cons(th, tt), Cons(oh, ot)) => Some(((Some(th()), Some(oh())), (tt(), ot())))
      case (Empty, Cons(oh, ot)) => Some(((None, Some(oh())), (Empty, ot())))
      case (Cons(th, tt), Empty) => Some(((Some(th()), None), (tt(), Empty)))
      case _ => None
    }

  lazy val tails: LazyList[LazyList[A]] = 
    LazyList.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((cons(h(), t()), t()))
    }.append(LazyList(empty))
  
  def hasSubsequence[B>:A](l: LazyList[B]): Boolean =
    tails.exists(_.startsWith(l))
  
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] = foldRight((init, LazyList(init))) ((a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
  )._2


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = {
    def f(n_1: Int, n: Int): LazyList[Int] = LazyList.cons(n_1, f(n, n_1 + n))
    f(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case Some((a, s)) => LazyList.cons(a, LazyList.unfold(s)(f))
    case _ => empty

  lazy val fibsViaUnfold: LazyList[Int] = LazyList.unfold((0, 1)) {
    case (current, next) => Some(current, (next, next + current))
  }

  def fromViaUnfold(n: Int): LazyList[Int] = LazyList.unfold(n)((s) => Some((s, s+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = LazyList.unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = LazyList.unfold(())(_ => Some(1, ()))
