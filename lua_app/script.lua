require("interf_lua")

m_open()
ierr = m_command("ff")

m_set(4,"i")
m_set( 0.34, "r")

a = {4,7,9}
m_set(a,3, "int1d")

b = {{1,2,3},{7,8,9}}
m_set( b, 3,2, "int2d")

r = {0.1, 0.2, 0.3, 0.4}
m_set( r, 4, "real1d" )

p = { {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3}, {3.1, 3.2, 3.3}, {4.1, 4.2, 4.3} }
m_set( p, 3, 4, "real2d" )

m_set("some lua string","string")

m_print()


ii = m_get("i")
print( "thing i", ii)

rr = m_get("r")
print( "thing r", rr )

i1d = m_get("int1d")
print( "thing i1d" )
for i,v in ipairs(i1d) do 
print(i,':',v)
end

i2d = m_get("int2d")
print("thing i2d" )
for i,m in ipairs(i2d) do
  for j,v in ipairs(m) do
     print('i:',i,'j:', j,'i2d(i,j):', v)
  end
end

r1d = m_get("real1d")
print( "thing real1d" )
for i,v in ipairs(r1d) do 
print(i,':',v)
end

r2d = m_get("real2d")
print("thing real2d" )
for i,m in ipairs(r2d) do
  for j,v in ipairs(m) do
     print('i:',i,'j:', j,'i2d(i,j):', v)
  end
end

ss = m_get("string")
print("thing string",ss)


m_close()


