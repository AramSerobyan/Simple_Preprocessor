# Simple_Preprocessor
A simple Preprocessor implemented in the Racket. It can tie functions to toknes (strings) and trigger them during the preprocessing. The main preprocessor takes string as an input and outputs the preprocessed string.

# Active Tokens 
An active token is a string which triggers a function. 

# Defining Active Tokens
Active tokens can be easily defined by the form ( define tokenName (params ...) body ... ). The existing def-active-token macro wil l do the necessary steps to make them active tokens.

# Process-String 
Preprocessor determines where is the first active token and triggers its function with the substring following the token. The preprocessor will repreprocess the string as long as active tokens will change the string.

# Existing active tokens.
There are few active tokens which are implemented. 
Var: It can infer a declaration of a type. for example var temp = new Type(); will be preprocessed to Type temp = new Type();
Alias: Can make an alias of two strings and change whenever they are met. e.g. alias ar = argument; int ar = 5; will become int argument = 5;
#: Allows advanced strings for printing. e.g. System.out.println(#" Example number #{1}" will ecome "); System.out.println(" Example number" + (1) + "");
