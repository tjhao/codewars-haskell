module Postfix where

begin istr          = istr []
push stack new istr = istr (new:stack)
add (x:y:ys) istr   = istr (x+y:ys)
end                 = head
