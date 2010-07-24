import org.junit._
import Assert._

import kdl.theory._

object Tools {
	def same(a:Double, b:Double):Unit = {
			assertTrue ( ( if (a>b) (a-b) else (b-a) ) < 0.0001 )
	}
	
	def same(a:V, b:V):Unit = {
		assertTrue ( (a-b).lengthSquared < 0.0001 )	
	}	
	
	def notSame(a:V, b:V):Unit = {
		assertTrue ( (a-b).lengthSquared > 0.0001 )	
	}	
}

class VTest {
	import Tools._

	val a = V(1,2,3);
	val aa = V(2,4,6);
	val b = V(2,3,4);
	val l = V(2,2,1);
	val o = V(0,0,0);
	val e1 = V(1,0,0);
	val e2 = V(0,1,0);
	val e3 = V(0,0,1);

	class Sphere(radius:Double) extends Function {
		def apply(r:V) = radius-r.length;
	}
	
	val s = new Sphere(1);
	
	//near, length
	@Test def equality() = {
		assertTrue( V(0,0,0).lengthSquared < 0.1  )
		same( a, a )
		notSame( b, a )
  }
	 
  // +, -, *, lengthSquared
   @Test def basics() = {
	  same( 3.0, l.length())
	  same( a+a , aa)
	  same( a.inversed(), a*(-1.0))
	  same( aa-a, a)
	  same( (a-a).length(), 0.0)
	  same( aa, a*2)
	  same( aa*aa, aa.lengthSquared())
	  same( a.midpoint(b) , (a+b)*0.5 )
	  same( aa.normalized() , aa / aa.length() )
	  same( a*3.0 , a/(1.0/3.0) )
  }
   
   // ^
   @Test def cross() = {
	  same( a ^ a, o)
	  val cross = a ^ l
	  same( (cross ^ l).length(), cross.length()*l.length() )
	  same( (cross ^ a).length(), cross.length()*a.length() )
	  same( cross*l, 0.0 )
	  same( cross*a, 0.0 )
	  same( e1 ^ e2, e3)
   }  
   
   @Test def function() = {
	  same( s(o), 1)
	  same( s(a.normalized()), 0)
	  ( s.find(l / 4, a)) match {
	 	  case None => assertTrue(false)
	 	  case Some(x) => same(x.length, 1 ) };
	  assertTrue( ( s.find(a, b) ) match { case None=>true 
	 	  	                               case Some(_)=>false }  )
	  same ( s.grad(a), a.normalized().inversed() )
   }  
}