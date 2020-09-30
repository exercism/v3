abstract class Fighter {

    /**
     * this method sets the default vulnerability to false for all the child classes.
     * @return the vulnerability i.e false.
     */
    boolean isVulnerable() {
        return false;
    }

    abstract int damagePoints(Fighter fighter);

}

class Warrior extends Fighter {

    @Override
    public String toString() {
        throw new UnsupportedOperationException("Please implement the toString() method with the required text");
    }

    @Override
    int damagePoints(Fighter wizard) {
        throw new UnsupportedOperationException("Please implement Warrior.damagePoints() method");
    }
}

class Wizard extends Fighter {

    @Override
    boolean isVulnerable() {
       throw new UnsupportedOperationException("Please implement Wizard.isVulnerable() method");
    }

    @Override
    int damagePoints(Fighter warrior) {
        throw new UnsupportedOperationException("Please implement Wizard.damagePoints() method");
    }

    void prepareSpell() {
        throw new UnsupportedOperationException("Please implement Wizard.prepareSpell() method");
    }

}
