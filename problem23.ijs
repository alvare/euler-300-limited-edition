fact =: ~.@q:
exp =: 0&(-.~)&(_&q:)
divcount =: 4 : '(<:x^(y+1))%(x-1)'
divcount2 =: ([: <: [ ^ 1 + ]) % 1 -~ [
divsum =: [: */ fact divcount exp
edivsum =: -~ divsum
abundant =: < edivsum
