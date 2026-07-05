
pub mod Test01 {

    use BasicParsers.Parse.{runParser, char, alt}

    @Test
    def test01(): Unit \ Assert = 
        let ans = runParser(char('A'), "ABC");
        Assert.assertEq(expected = Ok('A'), ans)
    
    @Test
    def test02(): Unit \ Assert = 
        let ans = runParser(alt(char('B'), char('A')), "ABC");
        Assert.assertEq(expected = Ok('A'), ans)

    @Test
    def test03(): Unit \ Assert = 
        let ans = runParser(_ -> { let a = char('A')(); let b = char('B')(); "${a}${b}" }, "ABC");
        Assert.assertEq(expected = Ok("AB"), ans)

}

