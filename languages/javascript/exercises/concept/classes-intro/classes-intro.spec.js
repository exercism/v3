import { Wizard, Wand } from "./classes-intro";

const wand_initialize_arr = [
    ["Dragon Heartstring", "Oak"]
]

const wizard_initialize_arr = [
    ["Dumbledore", "Purple", "Voldemort"]
]

describe("class-intro", () => {
    describe("wandTests", () => {
        it("can construct with a variety of options", () => {

            wand_initialize_arr.forEach(initializer => {
                const wand = new Wand(initializer[0], initializer[1]);

                expect(wand.core).toBe(initializer[0]);
                expect(wand.material).toBe(initializer[1])
            })
        })
        it("can do spells", () => {
            const wand = new Wand(...wand_initialize_arr[0]);

            const spell = "expelliarmus";

            expect(wand.useSpell(spell)).toBe(`Used spell ${spell}.`)
        })

        it("can display information based off of its properties", () => {

            wand_initialize_arr.forEach(([core, material]) => {
                const wand = new Wand(core, material);
                expect(wand.info()).toBe(`This wand is made of ${material}, and has a core of ${core}.`)
            })
        })
    })  

    describe("wizardTests", () => {
        it("can construct with a variety of options", () => {
            wizard_initialize_arr.forEach(([name, colour, nemesis]) => {
                const wizard = new Wizard(name, colour, nemesis);

                expect(wizard.name).toBe(name)
                expect(wizard.colour).toBe(colour)
                expect(wizard.nemesis).toBe(nemesis)
            })
        })

        it("can say hi", () => {
            wizard_initialize_arr.forEach(([name, colour, nemesis]) => {
                const wizard = new Wizard(name, colour, nemesis);

                expect(wizard.sayHi()).toBe(`Hi there, I'm ${name}. My favourite colour is ${colour} and I hate ${nemesis}.`)
            })
        })
    })
})