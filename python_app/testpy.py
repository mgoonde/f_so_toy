import py_interf

me = py_interf.thing()

import numpy as np

me.set("i",47)

v_int = np.array( [1,3,6,9] )
me.set("int1d", v_int )
me.set("string","this string looks like a snake (python?!), let's have some symbols /.,';][?><:{}|+_\=-)(*&^%$#@!)]")
me.set("r", np.sqrt(46.2) )

b=np.array([9.0, 4.3, -8.0])
me.set( "real1d", b )

me.set( "real2d", np.array([[1.0, 1.1, 1.2],[2.0, 2.1, 2.2 ],[3.0, 3.1, 3.2]]) )

v2=np.array([[1,2,3],[7,8,9]])
me.set("int2d", v2)
me.print()


c = 1.2/me.get("r")
print(1.2/np.sqrt(46.2), c)

ss=me.get("string")
print("got string:",ss)

gg=me.get("int2d")
print("got int2d:", gg.dtype)
print(gg)

v=np.matmul(gg, [0, 1, 1.5])
print(v)

v1 = np.matmul( me.get("real2d"), np.array([1.0, 0.3, -0.1]) )
print(v1)


# test error detection in py_interf
#err = me.command("this is an unknown command sent from python app!")

me.close()
