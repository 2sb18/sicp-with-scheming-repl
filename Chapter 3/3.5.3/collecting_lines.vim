let t=[] | %s/\<case\s\+\(\w\+\):\zs/\=add(t,submatch(1))[1:0]/g


