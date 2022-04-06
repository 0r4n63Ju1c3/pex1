# pex1
Pex1 for Crypto written in Haskell

I dont know why I decided to do it this way.

https://www.haskell.org/

To Run:
1. curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
	* Note that this is to install the entire Haskell toolchain
	* see https://www.haskell.org/downloads/ for more information

3. Install missing libraries using Hackage and Cabal
	- sudo cabal install "library name"
	- sudo cabal install primes
	- sudo cabal install exact-real
	- sudo cabal install numbers
	- sudo cabal install random

4. To call functions independently
	- ghci pex1.hs
	- > Main> main
	- follow prompts

5. if you want to compile use GHC
	- ghc pex1.hs

* executables should be included
* Contact Andrew Lee for any questions

# Documentation:
The only thing that I used was the slides and stack overflow to help
with haskell syntax since I am still trying to learn the language. I have listed
the links that I used below. Other than this, I only used the course resources
and help from Lt Col Merrit who told me to use ceiling instead of floor

https://hackage.haskell.org/package/numbers-3000.2.0.2/docs/Data-Number-CReal.html
https://stackoverflow.com/questions/49846233/dividing-by-huge-numbers-in-haskell
https://hackage.haskell.org/package/base-4.16.1.0/docs/System-Timeout.html
