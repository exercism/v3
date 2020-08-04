import org.junit.Test;
import org.junit.Ignore;

import static org.assertj.core.api.Assertions.assertThat;

public class IdentifierTest {

    @Test
    public void empty() {
        assertThat(Identifier.clean("")).isEmpty();
    }

    @Ignore("")
    @Test
    public void single_letter() {
        assertThat(Identifier.clean("A")).isEqualTo("A");
    }

    @Ignore
    @Test
    public void string() {
        assertThat(Identifier.clean("àḃç")).isEqualTo("àḃç");
    }

    @Ignore
    @Test
    public void spaces() {
        assertThat(Identifier.clean("my   Id")).isEqualTo("my___Id");
    }

    @Ignore
    @Test
    public void ctrl() {
        assertThat(Identifier.clean("my\0Id")).isEqualTo("myCTRLId");
    }

    @Ignore
    @Test
    public void string_with_no_letters() {
        assertThat(Identifier.clean("\uD83D\uDE00\uD83D\uDE00\uD83D\uDE00")).isEmpty();
    }

    @Ignore
    @Test
    public void kebab_to_camel_case() {
        assertThat(Identifier.clean("à-ḃç")).isEqualTo("àḂç");
    }

    @Ignore
    @Test
    public void omit_lower_case_greek_letters() {
        assertThat(Identifier.clean("MyΟβιεγτFinder")).isEqualTo("MyΟFinder");
    }

    @Ignore
    @Test
    public void combine_conversions() {
        assertThat(Identifier.clean("9 -abcĐ\uD83D\uDE00ω\0")).isEqualTo("_AbcĐCTRL");
    }
}
