package make_corpus;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

/**
 * Unit test for simple Startup.
 */
public class StartupTest
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public StartupTest(String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( StartupTest.class );
    }

    /**
     * Rigourous Test :-)
     */
    public void testApp()
    {
        String string = Startup.replaceAll("In the years thereafter, most of the Oil fields and platforms were named after pagan \u201cgods\u201d.", "\u201c", "\"");
        string = Startup.replaceAll(string, "\u201d", "\"");
        assertEquals("In the years thereafter, most of the Oil fields and platforms were named after pagan \"gods\".", string);
        assertTrue( true );
    }

    public void testNames() throws IOException {
        InputStream tokenNameFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        TokenizerModel tokenizerModel = null;
        TokenNameFinderModel tokenNameFinderModel = null;
        try {
            tokenizerModelIn = new FileInputStream("en-token.bin");
            tokenizerModel = new TokenizerModel(tokenizerModelIn);
            tokenNameFinderModelIn = new FileInputStream("en-ner-person.bin");
            tokenNameFinderModel = new TokenNameFinderModel(tokenNameFinderModelIn);
            Tokenizer tokenizer = new TokenizerME(tokenizerModel);
            NameFinderME nameFinder = new NameFinderME(tokenNameFinderModel);
            String str = Startup.replaceAllNames(tokenizer, nameFinder, "John Smith won the competition", "[name]");
            assertEquals("[name] won the competition", str);
            str = Startup.replaceAllNames(tokenizer, nameFinder, "Mary sent Jimmy Brown a letter.", "[name]");
            assertEquals("[name] sent [name] a letter.", str);
        } finally {
            if (tokenNameFinderModelIn != null) {
                try {
                    tokenNameFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (tokenizerModelIn != null) {
                try {
                    tokenizerModelIn.close();
                } catch (Exception e) {
                }
            }
        }
    }

}
