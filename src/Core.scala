package kdl.theory

//Interesting...
case class V(x:Double, y:Double, z:Double){
	import Math._
	 
	def length() = sqrt(x*x+y*y+z*z)
	def normalized() = this/this.length()
	def inversed() = V(-x, -y, -z);
	def lengthSquared() = x*x+y*y+z*z
	def +(that:V) = V(x+that.x, y+that.y, z+that.z)
	def -(that:V) = V(x-that.x, y-that.y, z-that.z)
	def *(times:Double) = V(x*times,y*times,z*times);
	def /(times:Double) = V(x/times,y/times,z/times);
	def *(that:V) = x*that.x + y*that.y + z*that.z;
	def ^(that:V) = V(y*that.z-z*that.y,
					  z*that.x-x*that.z,
					  x*that.y-y*that.x);
	def midpoint(that:V) = V( (x+that.x)/2, (y+that.y)/2, (z+that.z)/2);
	override def toString() = "(" + x.toString + "," + y.toString + "," +z.toString + ")"
}

trait Function{
	case class F(r:V, f:Double) //Function point
	val E = 0.0001

	def apply(r:V):Double;
	def point(r:V):F = F(r, this(r)) //Convenience, returns point+value.
	def find(a:V, b:V):Option[V] = {
		def findHelper(a:F, b:F, dist:Double ):Option[V] = {
			if (dist < E) return Some( a.r.midpoint(b.r) )
			if ( a.f * b.f > 0 ) return None // No NaN / +Inf...
			val m = this.point(a.r.midpoint(b.r));
			findHelper( if ( a.f * m.f < 0 ) a else b , m, dist/2);
		}
		findHelper(this.point(a), this.point(b), (a-b).length())
	}
	def grad(r:V):V = {
		val dx = this( r+V(E,0,0) ) - this( r-V(E,0,0) );
		val dy = this( r+V(0,E,0) ) - this( r-V(0,E,0) );
		val dz = this( r+V(0,0,E) ) - this( r-V(0,0,E) );
		V(dx/(2*E),dy/(2*E),dz/(2*E))
	}
}

class Cell(zero:V, unit:V) {
    def subdivide = {}
}