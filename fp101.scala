object Tel {

  val guard:PartialFunction[(Int,Int),Int] =  {
    case (1,x) => x
    case (0,_) => 1
  }

  def halfer(n:Int, x:Int):Int = n % 2 match {
    case 0 => telescope(n/2,x,guard,halfer,combine)
    case 1 => telescope((n-1)/2,x,guard,halfer,combine)
  }

  def combine(n:Int, x:Int, half:Int) = n % 2 match {
    case 0 => half * half
    case 1 => half * half * x   //i think this is right..
  }

  def telescope[N,T](n:N, x:T)(implicit guard:PartialFunction[(N,T),T], halfer: (N,T)=>T, combine:(N,T,T)=>T):T = {
    if(guard.isDefinedAt(n,x))
      guard(n,x)
    else{
      val half = halfer(n,x)
      combine(n,x,half)
    }
  }
}
