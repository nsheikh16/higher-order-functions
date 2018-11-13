object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] =  (lst1,lst2)  match {
    case (Nil,Nil)=> Nil
    case (Nil,h ::tail)=>Nil
    case (h::tail,Nil)=>Nil
    case( h :: tail, h2 :: tail2)  => f(h,h2) :: map2[A,B,C](f,tail,tail2)
    }

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1,lst2) match {
    case (Nil,Nil)=> Nil
    case (Nil,h ::tail)=>Nil
    case (h::tail,Nil)=>Nil
    case ( h :: tail, h2 :: tail2)  => (h,h2) :: zip[A,B](tail,tail2)
    }

  def flatten[A](lst: List[List[A]]): List[A] = lst match{
    case Nil=> Nil;
    case head :: tail => help[A]( head, flatten[A](tail))
    
    }

  def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match {
    case Nil=>Nil
    case h :: t =>  help[A](flatten[A](h),flatten3[A](t))
    }

  def help[A](l1: List[A], l2: List[A]) : List[A] = l1 match {
    case Nil => l2
    case h :: t=> h :: help[A](t,l2)
    }


 def buildList[A](length: Int, f: Int => A): List[A] = {
   if (length == 0) { 
     Nil}
   else if ( length > 0 ) {
      help[A](buildList[A](length-1,f), List[A](f(length-1)))}
      else {
        Nil}
   }

  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match {
    case Nil=>Nil
    case h :: t => help[B](f(h),mapList[A,B](t,f ))
    }
  

 def filtera[A](f: A => Boolean, alist : List[A]) : List[A] = alist match{
   case Nil=>Nil
   case h :: tail => f(h) match{
     case true => h :: filtera(f,tail)
     case false => List()
  }
 }

 def filterb[A](f: A => Boolean, alist : List[A]) : List[A] = alist match{
   case Nil=>Nil
   case h2 :: tail2 => f(h2) match{
     case false => h2 :: filterb(f,tail2)
     case true => List()
  }
  
 }
 def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = {
(filtera[A](f,lst), filterb[A](f,lst))
}

  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = alist1 match{
    case Nil=>alist2
    case h :: t => alist2 match{
    case Nil=>Nil
    case h2 :: t2 => {
      if (lessThan(h2,h)){
        h2 :: merge[A](lessThan,alist1,t2)
   }
   else  {
     h :: merge[A](lessThan,t,alist2)
     }
  
    }
 }
}
  def insert[A](lessThan: (A,A) => Boolean, x:A, alist : List[A]) : List[A] = alist match {
    case Nil => List(x)
    case h :: t => {
      if (lessThan(x,h)){
        x :: h :: t
        }
        else {
          h :: insert(lessThan,x, t)
          }
      }
    }

  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match{
    case Nil=> Nil
    case h :: t => insert(lessThan, h, sort(lessThan,t))
}
}
