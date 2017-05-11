package make_corpus;

/**
 * Created by serge on 5/11/2017.
 */

import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.postag.POSModel;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.tokenize.TokenizerModel;

import java.util.HashSet;

class Models {
    SentenceModel sentenceModel = null;
    TokenizerModel tokenizerModel = null;
    TokenNameFinderModel tokenNameFinderModel = null;
    POSModel posModel = null;
    HashSet<String> vocabulary = null;
}