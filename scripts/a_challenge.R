# Challenge!

# In this challenge, you will check to see if a package that you would like to load is currently on your computer. If it is not, you will install it. Once all packages are loaded, you will load the libraries in question.

# The magical if and else statements ------------------------------------------------

# if and else has special meaning in R. 

foo <- 'hello'

# If statement that evaluates to true:

if(foo == 'hello'){
  print('boy howdy')
} else {
  print('Hadley is dreamy')
}

# If statement that evaluates to false:

if(foo != 'hello'){
  print('boy howdy')
} else {
  print('Hadley is dreamy')
}

# Now let's consider a vector (1, 1, 2, 3):

v <-
  c(1, 1, 2, 3)

for (i in 1:length(v)) {
  if (v[i] == 2) {
    print('boy howdy')
  } else {
    print('foo')
  }
}

# The function installed.packages will return the packages currently installed on your computer:

installed.packages()

# What sort of object is this? (provide response in code)

# What are the names of the packages that are installed? (provide response in code)

# vector of packages you would like to install ----------------------------

package_names <-
  c(
    'tidyverse',
    'shiny',
    'lubridate',
    'leaflet',
    'RCurl'
  )

# the challenge -----------------------------------------------------------

# Write a function named "smartLibrary" that will check whether each package in package_names installed. If it is not, install it. Load the libraries in package_names.

