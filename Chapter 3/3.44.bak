; if you have balances in three accounts, and those balances are swap however many
; times, you're still going to have the same amounts of balances, maybe in some other
; order. Swapping balances is invariant with respect to the amount of the three balances

; now we're using the first version of the exchange, where there's 5 steps to it:
; 1. account #1 balance accessed
; 2. account #2 balance accessed
; 3. difference calculated #1 - #2
; 4. difference taken out of #1
; 5. difference added to #2

; How can this screw up?
; #1 has 100 in it
; #2 has 200 in it
; #3 has 300 in it

; Peter is going to exchange #1 and #2
; Paul is going to exchange #1 and #3

; Paul accesses #1 and #2 and calcuates diffence
; Peter access #1 and #3 and calculates diffence
; Paul does withdraw and deposit, #1=200 and #2=100
; Peter does a withdraw and deposit, #1=400 and #3=100
; now we have #1=400, #2 = 100, and #3=100
; we have the same sum of balances. The difference calcuated
; might not be up to date, but since it's being added to one account
; and taken from another, the sum of balances will still be invariant.

; obviously if we don't serialize anything, the sum of the balances will not be
; preserved. let's take the last example, 

; Paul accesses #1 and #2 and calcuates diffence
; Peter access #1 and #3 and calculates diffence
; Peter accesses #1 and gets 100
; Paul does withdraw and deposit, #1=200 and #2=100
; Peter does a withdraw and deposit, #1=300 and #3=100
; now we have #1=300, #2 = 100, and #3=100
; sum of balances is no longer invariant.