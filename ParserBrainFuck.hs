module ParserBrainFuck where

import Parser
import Types
----------- PARSER BrainFuck --------------------------------------------------

parserAdd,parserSub,parserInc,parserDec,parserOut,parserInp :: Parser Inst
parserAdd = const Add <$$> symbol' '+'

parserSub = const Sub <$$> symbol' '-'

parserInc = const Inc <$$> symbol' '>'

parserDec = const Dec <$$> symbol' '<'

parserOut = const Out <$$> symbol' '.'

parserInp = const Inp <$$> symbol' ',' 

parserLoop :: Parser Inst
parserLoop = f <$$> (symbol' '[') <**> parserMultInst <**> symbol' ']'
    where f _ b _ = Loop b

isComment = not . flip elem "+-><-,[]"

parserCmt :: Parser Inst
parserCmt = Cmt <$$> oneOrMore ( satisfy' isComment )

parserInst :: Parser Inst
parserInst = parserAdd 
         <|> parserSub
         <|> parserInc
         <|> parserDec
         <|> parserOut
         <|> parserInp
         <|> parserLoop
         <|> parserCmt

parserMultInst :: Parser BrainFuck 
parserMultInst = zeroOrMore parserInst
    
parserBrainFuck = fst . head . filter (null . snd) . parserMultInst

