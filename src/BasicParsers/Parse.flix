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
    
    use EffectExtras.Alt.Alt

    
    pub eff Parse { 
        def anyChar(): Option[Char]
        def peekChar(): Option[Char]
        def getPos(): Int32
        def setPos(i: Int32): Unit
        def atEnd(): Bool
        def takeString(i:Int32): Option[String]
    }


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

            def takeString(i, k) = {
                let pos = Ref.get(position);
                let end = pos + i;
                if (end <= endpos) 
                {
                    let ans = String.slice(start=pos, end=end, source);
                    Ref.put(end, position);
                    k(Some(ans))
                } else k(None)
            }
        }

    }

    pub def runParser(p: Unit -> a \ BasicParse + ef, s: String): Result[String, a] \ ef - Alt - Parse = 
        run {
            handle(s, p)()
        } with EffectExtras.Alt.runWithResult


    pub def alt(p: Unit -> a \ BasicParse + ef1, q: Unit -> a \ BasicParse + ef2): a \ BasicParse + ef1 + ef2 =
        EffectExtras.Alt.alt(p, q)


    pub def seqRight(p: Unit -> a \ BasicParse + ef1, q: Unit -> b \ BasicParse + ef2): b \ BasicParse + ef1 + ef2 =
        let _ = p();
        q()

    pub def seqLeft(p: Unit -> a \ BasicParse + ef1, q: Unit -> b \ BasicParse + ef2): a \ BasicParse + ef1 + ef2 =
        let ans = p();
        let _ = q();
        ans

    pub def eof(): Bool \ Parse = Parse.atEnd()

    pub def parseEof(): Unit \ BasicParse = 
        if (eof()) () else Alt.failure("parseEof - failed")

    pub def satisfy(test: Char -> Bool \ ef): Char \ BasicParse + ef = 
        match Parse.peekChar() {
            case Some(c) => if (test(c)) {let _ = Parse.anyChar(); c} else Alt.failure("satisfy")
            case _       => Alt.failure("satisfy")
        }

    pub def char(c: Char): Char \ BasicParse = 
        satisfy(c1 -> c1 == c)

    pub def literal(s: String): String \ BasicParse = 
        match Parse.takeString(String.length(s)) {
            case Some(s1) if s1 == s => s1
            case _                   => Alt.failure("literal not ${s}")
        }

    // Note - need to thunkify the alternatives supplied to `alt`...
    pub def aOrB(): Char \ BasicParse = 
        alt(_ -> char('a'), _ -> char('b'))

    pub def tryParse(p: Unit -> a \ BasicParse + ef): a \ BasicParse + ef = 
        let pos = getPos();
        alt(p, _ -> { setPos(pos); Alt.failure("")})


    pub def oneOf(cs: List[Char]): Char \ BasicParse =
        satisfy(c -> List.memberOf(c, cs))
    
    pub def noneOf(cs: List[Char]): Char \ BasicParse =
        satisfy(c -> not List.memberOf(c, cs))

    pub def upper(): Char \ BasicParse =
        satisfy(Char.isUpperCase)

    pub def lower(): Char \ BasicParse =
        satisfy(Char.isLowerCase)

    pub def many(p: Unit -> a \ BasicParse + ef): List[a] \ BasicParse + ef = 
        def loop(k) = {
            match EffectExtras.Alt.optional(p) {
                case Some(a) => loop(ks -> k(List.cons(a, ks)))
                case None => k(Nil)
            }
        };
        loop(identity)

    pub def many1(p: Unit -> a \ BasicParse + ef): List[a] \ BasicParse + ef = 
        let first = p();
        let tail = many(p);
        first :: tail

    pub def manyChar(p: Unit -> Char \ BasicParse + ef): String \ BasicParse + ef = 
        def loop(k) = {
            match EffectExtras.Alt.optional(p) {
                case Some(c) => loop(ks -> k(ToString.toString(c) +  ks))
                case None => k("")
            }
        };
        loop(identity)

    pub def many1Char(p: Unit -> Char \ BasicParse + ef): String \ BasicParse + ef = 
        let first = p();
        let tail = manyChar(p);
        ToString.toString(first) + tail

}

