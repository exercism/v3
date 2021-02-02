export class Wand {
    /**
     * The initialization function for a wand.
     * @param {string} core
     * @param {string} material
     * @memberof Wand
     * @constructor
     */
    // Create your constructor here!
    constructor(core, material) {
        this.core = core;
        this.material = material;
    }


    /**
     * A wand needs
     */
    spell(spell) {
        return `Spell used: ${spell}`
    }

    info() {
        return `This wand is made of ${this.material}, and has a core of ${this.core}.`        
    }
}

export class Wizard {
    /**
     *Creates an instance of Wizard.
     * @param {*} name
     * @param {*} colour
     * @param {*} nemesis
     * @memberof Wizard
     */
    constructor (name, colour, nemesis) {
        // Assign your properties here.
        this.name = name;
        this.colour = colour;
        this.nemesis = nemesis;
    }

    /**
     * This function returns a string saying, 
     *  "Hi, I'm <name>",
     * where <name> is the wizard's name.
     *
     * @memberof Wizard
     */
    sayHi() {
        return `Hi there, I'm ${this.name}. My favourite colour is ${this.colour} and I hate ${this.nemesis}.` 
    }
}