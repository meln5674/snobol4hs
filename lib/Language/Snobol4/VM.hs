{-|
Module          : Language.Snobol4.VM
Description     : TODO
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

TODO
-}
module Language.Snobol4.VM where


{-

Spitballing:

Idea: Use a stack machine

Executing a statement is broken into parts:
    1) Evaluating the subject
    2) Building the match pattern object
    3) Evaluating the replacement
    4) Matching the subject again the match pattern
    5) Performing match assignments
    6) Performing assignment for replacement
    7) Evaluating goto
    8) Jumping

Because each operator can be re-bound, each operator maps to a function.

-}

