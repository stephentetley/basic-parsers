/*
 * Copyright 2026 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


pub mod BasicParsers.Parse { 
    use BasicParsers.Parse.Parser
    use BasicParsers.Parse.satisfy


    use EffectExtras.Alt.Alt

    
    pub eff Parse { 
        def anyChar(): Option[Char]
        def peekChar(): Option[Char]
        def getPos(): Int32
        def setPos(i: Int32): Unit
        def atEnd(): Bool
        // def satisfies(f: String -> Option[(Int32, String)]): Option[String]
    }

    pub type alias Parser[a: Type, ef: Eff] = Unit -> a \ ef

    pub type alias BasicParse = Parse + Alt

    pub def handle(source: String, 
                    f: a -> b \ ef): a -> b \ (ef + Alt - Parse) = x -> region rc {
        let position = Ref.fresh(rc, 0);
        let endpos = String.length(source);
        run {
            f(x)
        } with handler Parse {
            
            def anyChar(k) = {
                let pos = Ref.get(position);
                match String.nth(pos, source) {
                    case Some(c) => {
                        Ref.put(pos + 1, position);
                        k(Some(c))
                    }
                    case _ => k(None)
                }
            }

            def peekChar(k) = {
                let pos = Ref.get(position);
                k(String.nth(pos, source))
            }

            def getPos(k) = {
                let pos = Ref.get(position);
                k(pos)
            }
            
            def setPos(i, k) = {
                Ref.put(i, position);
                k()
            }

            def atEnd(k) = {
                let pos = Ref.get(position);
                k(pos >= endpos)
            }

            // def satisfies(pred, k) = {
            //     let pos = Ref.get(position);
            //     match pred(String.slice(start=pos, end=endpos, source)) {
            //         case Some((consumed, ans)) => {
            //             Ref.put(pos + consumed, position);
            //             k(Some(ans))
            //         }
            //         case None => k(None)
            //     }
            // }
        }

    }

    pub def runParser(p: Parser[a, BasicParse + ef], s: String): Result[String, a] \ ef - Alt - Parse = 
        run {
            handle(s, p)()
        } with EffectExtras.Alt.runWithResult


    def answerOrFail(msg: String, sk: a -> ans \ ef, o: Option[a]): ans \ Alt + ef = match o {
        case Some(a) => sk(a)
        case None  => Alt.failure(msg)
    }


    pub def alt(p: Parser[a, BasicParse + ef1], q: Parser[a, BasicParse + ef2]): Parser[a, BasicParse + ef1 + ef2] = _ ->
        EffectExtras.Alt.alt(p, q)


    pub def eof(): Parser[Bool, Parse] = _ -> Parse.atEnd()

    pub def parseEof(): Parser[Unit, BasicParse] = _ ->
        if (eof()()) () else Alt.failure("parseEof - failed")

    pub def satisfy(test: Char -> Bool \ ef): Parser[Char, BasicParse + ef] = _ ->
        match Parse.peekChar() {
            case Some(c) => if (test(c)) Parse.anyChar() else None
            case _       => None
        } |> answerOrFail("satisfy", identity)

    pub def char(c: Char): Parser[Char, BasicParse] = 
        satisfy(c1 -> c1 == c)


    // pub def string(s: String): String \ BasicParse = 
    //     Parse.satisfies(input -> match String.sliceLeft(end = String.length(s), input) {
    //         case s1 if s1 == s => Some((1, s))
    //         case _                   => None
    //     }) |> answerOrFail("string - not ${s}", _ -> s)

    pub def aOrB(): Parser[Char, BasicParse] = 
        alt(char('a'), char('b'))

    pub def tryParse(p: Parser[a, BasicParse]): Parser[a, BasicParse] = _ ->
        let pos = getPos();
        alt(p, _ -> { setPos(pos); Alt.failure("")})()


    pub def oneOf(cs: List[Char]): Parser[Char, BasicParse] =
        satisfy(c -> List.memberOf(c, cs))
    
    pub def noneOf(cs: List[Char]): Parser[Char, BasicParse] =
        satisfy(c -> not List.memberOf(c, cs))

}

