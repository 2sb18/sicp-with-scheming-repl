; where you could have a deadlock situation

; you have a function (do-next-thing-in- queue) which accesses the queue, finds the
; next thing to be done, and does it. So it would first have to get a lock on the
; queue, then get a lock on the thing to do, complete it, remove it from the queue,
; release the lock on the thing to do, release the lock on the queue. Actually, I don't
; think this would be a problem. The locking is always serialized, first queue, then
; other resource.

; actually, maybe it is a problem.
; 1. get a lock on queue
; 2. find out next action on queue.
; 3. get a lock on next action.
; 4. do it.
; still doesn't seem like a problem.

; what if there is more than one queue?
; that might cause deadlock

; i think pointers might be like this. 

; (clear-if-even pointer-address)
; goes to pointer-address, goes to the address in there, then looks at that
; value and clears it if it's even.
; so first we have a serializer on each register. Get a lock on the pointer-address.
; find out where that points. get lock on place it points to. look at value and
; clear if it's even.

; say we called (clear-if-even 0xAA)
;               (clear-if-even 0xBB)

; and at address 0xAA=0xBB and at address 0xBB=0xAA. The first call wants to clear address
; 0xBB, s










