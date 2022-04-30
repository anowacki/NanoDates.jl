@testset "isequal" begin
    @test isequal(nd, nd)
    @test isequal(nd0, daytime)
    @test isequal(daytime, nd0)
    @test isequal(nd00, dayt)
    @test isequal(dayt, nd00)

    @test !isequal(nd, laternanodate)
    @test !isequal(nd, daytime)
    @test !isequal(daytime, nd)
    @test !isequal(nd, dayt)
    @test !isequal(dayt, nd)
end

@testset "isless" begin
    @test isless(nd, laternanodate)
    @test isless(nd, laterdatetime)
    @test isless(nd, laterdate)
    @test isless(earlierdatetime, nd)
    @test isless(earlierdate, nd)
end

@testset "(==, !=)" begin
    @test ==(nd, nd)
    @test ==(nd0, daytime)
    @test ==(daytime, nd0)
    @test ==(nd00, dayt)
    @test ==(dayt, nd00)

    @test !=(nd, laternanodate)
    @test !=(nd, daytime)
    @test !=(daytime, nd)
    @test !=(nd, dayt)
    @test !=(dayt, nd)
end

@testset "(<, >=)" begin
    @test <(nd, laternanodate)
    @test <(nd, laterdatetime)
    @test <(nd, laterdate)
    @test <(earlierdatetime, nd)
    @test <(earlierdate, nd)

    @test >=(laternanodate, nd)
    @test >=(laterdatetime, nd)
    @test >=(laterdate, nd)
    @test >=(nd, earlierdatetime)
    @test >=(nd, earlierdate)

    @test >=(nd, nd)
    @test >=(nd0, daytime)
    @test >=(daytime, nd0)
    @test >=(nd00, dayt)
    @test >=(dayt, nd00)
end

@testset "(>, <=)" begin
    @test >(laternanodate, nd)
    @test >(laterdatetime, nd)
    @test >(laterdate, nd)
    @test >(nd, earlierdatetime)
    @test >(nd, earlierdate)

    @test <=(nd, laternanodate)
    @test <=(nd, laterdatetime)
    @test <=(nd, laterdate)
    @test <=(earlierdatetime, nd)
    @test <=(earlierdate, nd)

    @test <=(nd, nd)
    @test <=(nd0, daytime)
    @test <=(daytime, nd0)
    @test <=(nd00, dayt)
    @test <=(dayt, nd00)
end