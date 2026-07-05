
pub mod Test01 {

    use BasicParsers.Parse.{runParser, char, alt, seqRight, many1Char, upper,
        literal}

    @Test
    def test01(): Unit \ Assert = 
        let ans = runParser(_ -> char('A'), "ABC");
        Assert.assertEq(expected = Ok('A'), ans)
    
    @Test
    def test02(): Unit \ Assert = 
        let ans = runParser(_ -> alt(_ -> char('B'), _ -> char('A')), "ABC");
        Assert.assertEq(expected = Ok('A'), ans)

    @Test
    def test03(): Unit \ Assert = 
        let ans = runParser(_ -> { let a = char('A'); let b = char('B'); "${a}${b}" }, "ABC");
        Assert.assertEq(expected = Ok("AB"), ans)

    @Test
    def seqRight01(): Unit \ Assert = 
        let ans = runParser(_ -> seqRight(_ -> char('A'), _ -> char('B')), "ABC");
        Assert.assertEq(expected = Ok('B'), ans)
    
    @Test
    def many1Char01(): Unit \ Assert = 
        let ans = runParser(_ -> many1Char(upper), "ABC EFG");
        Assert.assertEq(expected = Ok("ABC"), ans)

    @Test
    def literal01(): Unit \ Assert = 
        let ans = runParser(_ -> literal("ABC"), "ABC EFG");
        Assert.assertEq(expected = Ok("ABC"), ans)

}

