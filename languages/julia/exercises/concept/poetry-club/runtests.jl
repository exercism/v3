using Test

include("poetry-club.jl")

@testset "Front door responses" begin
    @testset "'Shire' by Michael Lockwood" begin
        @test front_response("Stands so high") == 'S'
        @test front_response("Huge hooves too") == 'H'
        @test front_response("Impatiently waits for") == 'I'
        @test front_response("Reins and harness") == 'R'
        @test front_response("Eager to leave") == 'E'
    end

    @testset "'Summer' by John Albert Caballero" begin
        @test front_response("Sunshine warming my toes,") == 'S'
        @test front_response("Underwater fun with my friends.") == 'U'
        @test front_response("Making homemade ice cream on the porch,") == 'M'
        @test front_response("Many long nights catching fireflies.") == 'M'
        @test front_response("Early morning walks to the creek,") == 'E'
        @test front_response("Reveling in the freedom of lazy days.") == 'R'
    end

    @testset "'Sophia' by John Albert Caballero" begin
        @test front_response("Serene, calming quality") == 'S'
        @test front_response("Organized, you always have it together") == 'O'
        @test front_response("Picturesque, strikingly beautiful") == 'P'
        @test front_response("Honest, so genuine") == 'H'
        @test front_response("Imaginative, a creative mind") == 'I'
        @test front_response("Alluring, so attractive") == 'A'
    end

    @testset "'Code' by Derk-Jan Karrenbeld" begin
        @test front_response("Compilers intensily bestow") == 'C'
        @test front_response("On commencing without ego") == 'O'
        @test front_response("Different processes ajar") == 'D'
        @test front_response("Exit with zero quick") == 'E'
    end

    @testset "Unknown" begin
        @test front_response("Ιησούς") == 'Ι'
        @test front_response("Χριστός") == 'Χ'
        @test front_response("Θεού") == 'Θ'
        @test front_response("Υἱός") == 'Υ'
        @test front_response("Σωτήρ") == 'Σ'
    end

    @testset "'Подпись' by И. Чудасовым" begin
        @test front_response("И если тексты все прочли") == 'И'
        @test front_response("Вы, то поймете: я ворчлив,") == 'В'
        @test front_response("А жизнь моя не так легка.") == 'А'
        @test front_response("Но мил моей судьбы капкан.") == 'Н'
    end
end

@testset "Front door passwords" begin
     # 'Shire' by Michael Lockwood
    @test front_password("""
        Stands so high
        Huge hooves too
        Impatiently waits for
        Reins and harness
        Eager to leave"""
    ) == "Shire"

     # 'Summer' by John Albert Caballero
    @test front_password("""
        Sunshine warming my toes,
        Underwater fun with my friends.
        Making homemade ice cream on the porch,
        Many long nights catching fireflies.
        Early morning walks to the creek,
        Reveling in the freedom of lazy days."""
    ) == "Summer"

     # 'Sophia' by John Albert Caballero
    @test front_password("""
        Serene, calming quality
        Organized, you always have it together
        Picturesque, strikingly beautiful
        Honest, so genuine
        Imaginative, a creative mind
        Alluring, so attractive"""
    ) == "Sophia"

     # 'Code' by Derk-Jan Karrenbeld
    @test front_password("""
        Compilers intensily bestow
        On commencing without ego
        Different processes ajar
        Exit with zero quick"""
    ) == "Code"

     # Unknown
    @test front_password("""
        Ιησούς
        Χριστός
        Θεού
        Υἱός
        Σωτήρ"""
    ) == "Ιχθυσ"

     # 'Подпись' by И. Чудасовым
    @test front_password("""
        И если тексты все прочли
        Вы, то поймете: я ворчлив,
        А жизнь моя не так легка.
        Но мил моей судьбы капкан."""
    ) == "Иван"
end

@testset "Back door responses" begin
    @testset "'Shire' by Michael Lockwood" begin
        @test back_response("Stands so high") == 'h'
        @test back_response("Huge hooves too") == 'o'
        @test back_response("Impatiently waits for") == 'r'
        @test back_response("Reins and harness") == 's'
        @test back_response("Eager to leave") == 'e'
    end

    @testset "'Code' by Derk-Jan Karrenbeld" begin
        @test back_response("Compilers intensily bestow") == 'w'
        @test back_response("On commencing without ego") == 'o'
        @test back_response("Different processes ajar") == 'r'
        @test back_response("Exit with zero quick") == 'k'
    end

    @testset "'Подпись' by И. Чудасовым" begin
        @test back_response("И если тексты все прочли") == 'и'
        @test back_response("Вы, то поймете: я ворчлив,") == 'в'
        @test back_response("А жизнь моя не так легка.") == 'а'
        @test back_response("Но мил моей судьбы капкан.") == 'н'
    end
end

@testset "Back door passwords" begin
     # 'Shire' by Michael Lockwood
    @test back_password("""
        Stands so high
        Huge hooves too
        Impatiently waits for
        Reins and harness
        Eager to leave"""
    ) == "Horse, please."

     # 'Code' by Derk-Jan Karrenbeld
    @test back_password("""
        Compilers intensily bestow
        On commencing without ego
        Different processes ajar
        Exit with zero quick"""
    ) == "Work, please."

     # 'Подпись' by И. Чудасовым
    @test back_password("""
        И если тексты все прочли
        Вы, то поймете: я ворчлив,
        А жизнь моя не так легка.
        Но мил моей судьбы капкан."""
    ) == "Иван, please."
end
