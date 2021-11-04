= cl-package-locks overview =

This library aims to provide a uniform way of dealing with 
package-locks across those common lisp implementations that
provide them.  If a given implementation does not provide a 
package-lock mechanism then no action will be taken by the 
use of this library.

= Motivation =

In loading LISA I noticed a package lock violation during 
compilation.  I was interested in correcting the violation 
since I assume it's the reason the package isn't included 
in quicklisp, etc.  

I found the package lock violation was due to a class reader 
function being named CLASS-NAME, which would add a method to 
the package-lock protected CLASS-NAME generic-function.

I thought about modifing the name of the function, but then 
realized that the LISA has been around a while and how altering
the name might cause compatability issues (however small).

So I decided I'd hack in some code to disable the package-lock
and after dealing with sbcl, cmucl, AllegroCL.. I realized this
might be better suited for a general purpose library, so I 
created this small lib to accomplish the task.

In doing some early research to see if others had delt with this
issue I discovered this thread:

  http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2009-02/msg01372.html

This library distinguishes itself from the above by not trying
to eleminiate redefinition warnings or use the native
without-package-locks macro's provided by most package-locking
implementations.  It also provides some general use functions
and macros that might be useful in dealing with package locking.

I find this library to satisfy my needs with loading LISA and I
hope it suits others in their moments of package-lock violation 
need :)

= Status =

Barely coded and tested using sbcl.  Needs more testing on 
AllegroCL, clisp, and cmucl.  I'm also interested in creating 
a uniform way of dealing with package-lock errors and providing 
implementation agnostic restarts.  Another important piece is 
looking at how the various implementations implement 
without-package-locks to see if I'm missing anything important .

= A Brief HowTo =

(in-package :cl-package-locks)

; Testing if packages are locked
(package-locked-p :cl-user)

; Locking a package
(lock-package (find-package :cl-user))

; Unlocking a package, notice that packages can be described 
; in several different ways (currently symbol & package).
(unlock-package 'cl-user)

; Working on lists of packages.  The RESOLVE-PACKAGES function
; converts a list of package descriptors to packages or errors.
(let ((pkgs (resolve-packages '(:cl-package-locks :swank))))
  (lock-packages pkgs)
  ; your code here
  (unlock-packages))

; Temporarily unlocking a list of packages
(with-packages-unlocked (cl mop)
  ; something dangerous
  t)

; Temporarily unlock all packages
(without-package-locks
  ; something dangerous
  nil)

; List all unlocked packages
(all-unlocked-packages)

; List all locked packages
(all-locked-packages)
