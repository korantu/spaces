package kdl.theory

//Interesting...
case class V(x:Double, y:Double, z:Double){
	import Math._
	 
	def length() = sqrt(x*x+y*y+z*z)
	def lengthSquared() = x*x+y*y+z*z
	def +(that:V) = V(x+that.x, y+that.y, z+that.z)
	def -(that:V) = V(x-that.x, y-that.y, z-that.z)
	def *(times:Double) = V(x*times,y*times,z*times);
	def *(that:V) = x*that.x + y*that.y + z*that.z;
	def ^(that:V) = V(y*that.z-z*that.y,
					  z*that.x-x*that.z,
					  x*that.y-y*that.x);
}
 