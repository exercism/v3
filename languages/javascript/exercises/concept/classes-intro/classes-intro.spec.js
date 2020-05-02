import { Wizard, Wand } from "./classes-intro";

const initialize_arr = [
    ["Dragon Heartstring", "Oak"]
]

describe("class-intro", () => {
    describe("wandTests", () => {
        it("can construct with a variety of options", () => {

            initialize_arr.forEach(initializer => {
                const wand = new Wand(initializer[0], initializer[1]);

                expect(wand.core).toBe(initializer[0]);
                expect(wand.material).toBe(initializer[1])
            })
        })
        it("can do spells", () => {
            const wand = new Wand(...initialize_arr[0]);

            const spell = "expelliarmus";

            expect(wand.useSpell(spell)).toBe(`Used spell ${spell}.`)
        })

        it("can display information based off of its properties", () => {

            initialize_arr.forEach(([core, material]) => {
                const wand = new Wand(core, material);
                expect(wand.info()).toBe(`This wand is made of ${material}, and has a core of ${core}.`)
            })
        })
    })  
})