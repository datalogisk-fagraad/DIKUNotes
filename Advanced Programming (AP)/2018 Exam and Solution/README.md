## How to run (some of) the code
The 5 min guide for getting the Haskell-part up and running is as follows:
1. Install Haskell and the stack tool. The easy way is just installing the haskell-platform from [here](https://www.haskell.org/downloads/) as this should give you everything you need in one go.
For Ubuntu users this is done by
```
sudo apt install haskell-platform
```
2. For the 2018 course, a specific version of the compiler was required to avoid problems. Set this up by typing this in the terminal
```
stack setup --resover lts-12.6
```
This will set up the environment to the (2018) right GHC version.
3. Run tests by navigating to `handin/appm` and run 
```
stack test
```
this will eventually run all the tests, after installing the required packages etc.
