module ParserBrainFuck where

import Parser
import Types

----------- PARSER BrainFuck --------------------------------------------------

parserAdd,parserSub,parserInc,parserDec,parserOut,parserInp,parserLoop,parserInst,parserCmt :: Parser Inst

parserAdd = const Add <$$> symbol' '+'
parserSub = const Sub <$$> symbol' '-'
parserInc = const Inc <$$> symbol' '>'
parserDec = const Dec <$$> symbol' '<'
parserOut = const Out <$$> symbol' '.'
parserInp = const Inp <$$> symbol' ',' 

isComment = not . flip elem "+-><-,[]"
parserCmt = Cmt <$$> oneOrMore ( satisfy' isComment )

parserLoop = f <$$> symbol' '[' <**> parserMultInst <**> symbol' ']'
    where f _ b _ = Loop b


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
    
parserBrainFuck :: String -> BrainFuck
parserBrainFuck = fst . head . filter (null . snd) . parserMultInst

