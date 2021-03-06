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
    TokenNameFinderModel personNameFinderModel = null;
    TokenNameFinderModel organizationNameFinderModel = null;
    TokenNameFinderModel timeFinderModel = null;
    TokenNameFinderModel dateFinderModel = null;
    TokenNameFinderModel locationModel = null;
    POSModel posModel = null;
    String[] vocabulary = null;
}