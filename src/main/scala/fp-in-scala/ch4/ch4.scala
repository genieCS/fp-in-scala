package ch4

// 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = (this map f).getOrElse(None)
  def getOrElse[B >: A](defualt: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = Some(this) getOrElse ob
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) && f(a) => Some(a)
    else _ => _
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

//4.2
def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
val absO: Option[Double] => Option[Double] = lift(math.abs)

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

def parseInsuranceRateQuote(
  age: String,
  numberOfSpeedTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}

//4.3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa,bb)))

//4.4
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.find(e == None).map(a.map(Some(a) => a))

def parseInts(a: List[String]): Option[List[Int]] =
  sequence( a map (i => Try(i.toInt)))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case x::xs => f(x).flatMap(xx => traverse(xs).map(xs => xx::xs))
  case Nil => Some(Nil)
}