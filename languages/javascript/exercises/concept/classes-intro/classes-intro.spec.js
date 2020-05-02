import { Wizard, Wand } from "./classes-intro";

describe("class-intro", () => {
    describe("canConstructWand", () => {
        it("can construct with a variety of options", () => {
            const initialize_array = [
                ["Dragon Heartstring", "Oak"]
            ]

            initialize_array.forEach(initializer => {
                const wand = new Wand(initializer[0], initializer[1]);

                expect(wand.core).toBe(initializer[0]);
                expect(wand.material).toBe(initializer[1])
            })
        })
    })  
})