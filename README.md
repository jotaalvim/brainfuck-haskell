# BrainFuck Interpreter

This project was made using standard parsing combinators defined in `Parser.hs`. 

### BrainFuck State Machine

A BrainFuck structure is defined as a tuple consisting of a data pointer and a list of integers, which represent the memory data.

```
type BrainFuck = ( Int , [Int] )
```




# Usage
```
ghci> brainFuck "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." 
Hello World!                                     
[0,0,72,100,87,33,10,0,0,0,0,0,0,0,0,0,0,0,0,0]
```

