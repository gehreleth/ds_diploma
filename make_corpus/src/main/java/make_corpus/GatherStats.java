package make_corpus;

import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.util.Span;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Created by serge on 5/10/2017.
 */
public class GatherStats {

    public static final int MAX_NGRAM = 6;

    static {
        try {
            Class.forName("org.sqlite.JDBC");
            //c = DriverManager.getConnection("jdbc:sqlite:test.db");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static final Pattern SQ_BRACES = Pattern.compile("\\[([^]]+)]");

    public static void main(String[] args) throws IOException, SQLException {
        new Thread(new Runnable() {
            public void run() {
                try {
                    threadMain("../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.stats.db");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
        new Thread(new Runnable() {
            public void run() {
                try {
                    threadMain("../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.stats.db");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
        new Thread(new Runnable() {
            public void run() {
                try {
                    threadMain("../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.stats.db");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
    }

    public static void threadMain(String srcFile, String destFile) throws IOException, SQLException{
        InputStream sentenceModelModelIn = null;
        InputStream personFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        InputStream posModelIn = null;
        InputStream orgNameFinderModelIn = null;
        InputStream timeFinderModelIn = null;
        InputStream dateFinderModelIn = null;
        InputStream locationModelIn = null;
        try {
            Models models = new Models();
            sentenceModelModelIn = new FileInputStream("en-sent.bin");
            models.sentenceModel = new SentenceModel(sentenceModelModelIn);
            tokenizerModelIn = new FileInputStream("en-token.bin");
            models.tokenizerModel = new TokenizerModel(tokenizerModelIn);
            personFinderModelIn = new FileInputStream("en-ner-person.bin");
            models.personNameFinderModel = new TokenNameFinderModel(personFinderModelIn);
            orgNameFinderModelIn = new FileInputStream("en-ner-organization.bin");
            models.organizationNameFinderModel = new TokenNameFinderModel(orgNameFinderModelIn);
            timeFinderModelIn = new FileInputStream("en-ner-time.bin");
            models.timeFinderModel = new TokenNameFinderModel(timeFinderModelIn);
            dateFinderModelIn = new FileInputStream("en-ner-date.bin");
            models.dateFinderModel = new TokenNameFinderModel(dateFinderModelIn);
            locationModelIn = new FileInputStream("en-ner-location.bin");
            models.locationModel = new TokenNameFinderModel(locationModelIn);

            posModelIn = new FileInputStream("en-pos-maxent.bin");
            models.posModel = new POSModel(posModelIn);

            processSingleFile(models, srcFile, destFile);
        } finally {
            if (locationModelIn != null) {
                try {
                    locationModelIn.close();
                } catch (Exception e) {
                }
            }
            if (orgNameFinderModelIn != null) {
                try {
                    orgNameFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (timeFinderModelIn != null) {
                try {
                    timeFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (dateFinderModelIn != null) {
                try {
                    dateFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (posModelIn != null) {
                try {
                    posModelIn.close();
                } catch (Exception e) {
                }
            }
            if (personFinderModelIn != null) {
                try {
                    personFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (tokenizerModelIn != null) {
                try {
                    tokenizerModelIn.close();
                } catch (Exception e) {
                }
            }
            if (sentenceModelModelIn != null) {
                try {
                    sentenceModelModelIn.close();
                } catch (Exception e) {
                }
            }
        }
    }

    public static String replaceAll(String source, CharSequence seq, CharSequence replacement) {
        int p1, p0 = 0;
        String seq0 = String.valueOf(seq);
        StringBuilder retVal = new StringBuilder();
        do {
            p1 = source.indexOf(seq0, p0);
            retVal.append(p1 != -1 ? source.substring(p0, p1) : source.substring(p0));
            if (p1 != -1) {
                retVal.append(replacement);
                p0 = p1 + seq.length();
            } else
                p0 = p1;
        } while (p0 != -1);
        return retVal.toString();
    }

    private static Span checkNameSpan(Span[] namespans, int tokenNo) {
        if (namespans == null)
            return null;

        for (Span namespan : namespans) {
            if (namespan.contains(tokenNo))
                return namespan;
        }

        return null;
    }

    /*
    1.	CC	Coordinating conjunction
	2.	CD	Cardinal number
	3.	DT	Determiner
	4.	EX	Existential there
	5.	FW	Foreign word
	6.	IN	Preposition or subordinating conjunction
	7.	JJ	Adjective
	8.	JJR	Adjective, comparative
	9.	JJS	Adjective, superlative
	10.	LS	List item marker
	11.	MD	Modal
	12.	NN	Noun, singular or mass
	13.	NNS	Noun, plural
	14.	NNP	Proper noun, singular
	15.	NNPS	Proper noun, plural
	16.	PDT	Predeterminer
	17.	POS	Possessive ending
	18.	PRP	Personal pronoun
	19.	PRP$	Possessive pronoun
	20.	RB	Adverb
	21.	RBR	Adverb, comparative
	22.	RBS	Adverb, superlative
	23.	RP	Particle
	24.	SYM	Symbol
	25.	TO	to
	26.	UH	Interjection
	27.	VB	Verb, base form
	28.	VBD	Verb, past tense
	29.	VBG	Verb, gerund or present participle
	30.	VBN	Verb, past participle
	31.	VBP	Verb, non-3rd person singular present
	32.	VBZ	Verb, 3rd person singular present
	33.	WDT	Wh-determiner
	34.	WP	Wh-pronoun
	35.	WP$	Possessive wh-pronoun
	36.	WRB	Wh-adverb
     */
    public static void processSingleFile(Models models, String inputFileName, String outputFileName) throws IOException, SQLException {
        BufferedReader reader = null;
        Connection conn = null;
        try {
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(models.sentenceModel);
            Tokenizer tokenizer = new TokenizerME(models.tokenizerModel);
            NameFinderME personFinder = new NameFinderME(models.personNameFinderModel);
            NameFinderME orgFinder = new NameFinderME(models.organizationNameFinderModel);
            NameFinderME timeFinder = new NameFinderME(models.timeFinderModel);
            NameFinderME dateFinder = new NameFinderME(models.dateFinderModel);
            NameFinderME locationFinder = new NameFinderME(models.locationModel);

            POSTaggerME tagger = new POSTaggerME(models.posModel);

            conn = initializeDatabase(outputFileName);
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFileName)));
            int count = 0;
            ArrayList<String> outToks = new ArrayList<String>();
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences = sentenceDetector.sentDetect(line);
                for (String rawSentence : sentences) {
                    System.out.printf("\r Sentence : %d", ++count);
                    rawSentence = replaceAll(rawSentence, "\u201c", "\"");
                    rawSentence = replaceAll(rawSentence, "\u201d", "\"");
                    rawSentence = replaceAll(rawSentence, "\u2019", "\'");
                    rawSentence = replaceAll(rawSentence, "wanna", "want to");
                    rawSentence = replaceAll(rawSentence, "gonna", "going to");

                    rawSentence = rawSentence.replaceAll("[^\\p{ASCII}]", "");

                    String[] sentence = tokenizer.tokenize(rawSentence);
                    String postags[] = tagger.tag(sentence);

                    Span[] personSpan = personFinder.find(sentence);
                    personFinder.clearAdaptiveData();

                    Span[] orgSpan = orgFinder.find(sentence);
                    orgFinder.clearAdaptiveData();

                    Span[] timeSpan = timeFinder.find(sentence);
                    timeFinder.clearAdaptiveData();

                    Span[] dateSpan = dateFinder.find(sentence);
                    dateFinder.clearAdaptiveData();

                    Span[] locationSpan = locationFinder.find(sentence);
                    locationFinder.clearAdaptiveData();

                    ArrayList<Span> templateSpans0 = new ArrayList<Span>();
                    templateSpans0.addAll(Arrays.asList(personSpan));
                    templateSpans0.addAll(Arrays.asList(orgSpan));
                    templateSpans0.addAll(Arrays.asList(timeSpan));
                    templateSpans0.addAll(Arrays.asList(dateSpan));
                    templateSpans0.addAll(Arrays.asList(locationSpan));

                    Span[] templateSpans = templateSpans0.toArray(new Span[templateSpans0.size()]);
                    outToks.clear();
                    outToks.add("#b");
                    for (int i = 0; i < sentence.length; i++) {
                        String token = sentence[i];
                        String prevToken = null;

                        {   int ix = outToks.size() - 1;
                            if (ix >= 0) {
                                prevToken = outToks.get(ix);
                            }
                        }

                        String posTag = postags[i];

                        Span templateSpan = checkNameSpan(templateSpans, i);
                        if (templateSpan != null) {
                            i = templateSpan.getEnd() - 1;
                            token = StringUtils.join(ArrayUtils.subarray(sentence, templateSpan.getStart(), templateSpan.getEnd()), " ");
                        }

                        if (templateSpan == null) {
                            if (!token.startsWith("NNP")) {
                                token = token.toLowerCase().replaceAll("\\p{Punct}", " ").replaceAll("\\s+", " ").trim();
                            }
                            if ("s".equalsIgnoreCase(token)) {
                                if ("VBZ".equals(posTag)) {
                                    token = "is";
                                } else if (prevToken != null) {
                                    int ix = outToks.size() - 1;
                                    outToks.set(ix, prevToken + "'s");
                                    continue; // We aren't going to introduce a new token here, so let's skip iteration.
                                }
                            } else if ("n t".equalsIgnoreCase(token) && "RB".equals(posTag)) {
                                token = "not";
                                if (prevToken != null) {
                                    int ix = outToks.size() - 1;
                                    if ("ca".equalsIgnoreCase(prevToken)) {
                                        outToks.set(ix, "can");
                                    } else if ("ai".equalsIgnoreCase(prevToken)) {
                                        outToks.set(ix, prevToken + "n't");
                                        continue; // We aren't going to introduce a new token here, so let's skip iteration.
                                    }
                                }
                            } else if ("ve".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                                token = "have";
                            } else if ("re".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                                token = "are";
                            } else if ("m".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                                token = "am";
                            } else if ("ll".equalsIgnoreCase(token) && "MD".equals(posTag)) {
                                token = "will";
                            } else if ("d".equalsIgnoreCase(token) && ("MD".equals(posTag) || "VBD".equals(posTag))) {
                                token = "would";
                            } else if ("u".equalsIgnoreCase(token) && "PRP".equals(posTag)) {
                                token = "you";
                            }

                            if (posTag.startsWith("N") || posTag.startsWith("V") || posTag.startsWith("J")) {
                                if (token.indexOf(' ') == -1) {
                                    outToks = addToken(outToks, token);
                                } else {
                                    String[] lcs = token.split("\\s");
                                    for (String lc0 : lcs) {
                                        outToks = addToken(outToks, lc0);
                                    }
                                }
                            } else if (posTag.startsWith("LS") || posTag.startsWith("CD")) {
                                outToks = addToken(outToks,"#number");
                                updateMacroStats(conn, "number", token);
                            } else {
                                outToks = addToken(outToks, token);
                            }
                        } else {
                            updateMacroStats(conn, templateSpan.getType(), token);
                            outToks = addToken(outToks,"#" + templateSpan.getType());
                        }
                    }
                    outToks.add("#e");
                    storeNgreams(conn, outToks);
                }
            }
            conn.commit();
            createIndices(conn);
            aggregateRecords(conn);
            dropTempTables(conn);
       } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (Exception e) {
                }
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                }
            }
        }
    }

    private static void updateMacroStats(Connection conn, String type, String text) throws SQLException {
        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into " + type + "_tmp(name) values(?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, text);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try {
                stmt.close();
            } catch (Exception e) {
            }
        }
    }

    private static void dropTempTables(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Drop n1gram_tmp...");
            stmt.executeUpdate("DROP TABLE n1gram_tmp");
            System.out.println("Drop n2gram_tmp...");
            stmt.executeUpdate("DROP TABLE n2gram_tmp");
            System.out.println("Drop n3gram_tmp...");
            stmt.executeUpdate("DROP TABLE n3gram_tmp");
            System.out.println("Drop n4gram_tmp...");
            stmt.executeUpdate("DROP TABLE n4gram_tmp");
            System.out.println("Drop n5gram_tmp...");
            stmt.executeUpdate("DROP TABLE n5gram_tmp");
            System.out.println("Drop n6gram_tmp...");
            stmt.executeUpdate("DROP TABLE n6gram_tmp");
            System.out.println("Drop person_tmp...");
            stmt.executeUpdate("DROP TABLE person_tmp");
            System.out.println("Drop number_tmp...");
            stmt.executeUpdate("DROP TABLE number_tmp");
            System.out.println("Drop organization_tmp...");
            stmt.executeUpdate("DROP TABLE organization_tmp");
            System.out.println("Drop location_tmp...");
            stmt.executeUpdate("DROP TABLE location_tmp");
            System.out.println("Drop time_tmp...");
            stmt.executeUpdate("DROP TABLE time_tmp");
            System.out.println("Drop date_tmp...");
            stmt.executeUpdate("DROP TABLE date_tmp");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void aggregateRecords(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Aggregate n1gram ...");
            stmt.executeUpdate("CREATE TABLE n1gram AS SELECT w1, count(1) - 1 as `count` FROM n1gram_tmp GROUP BY w1 HAVING `count` > 0");
            System.out.println("Aggregate n2gram ...");
            stmt.executeUpdate("CREATE TABLE n2gram AS SELECT w1, w2, count(1) - 1 as `count` FROM n2gram_tmp GROUP BY w1, w2  HAVING `count` > 0");
            System.out.println("Aggregate n3gram ...");
            stmt.executeUpdate("CREATE TABLE n3gram AS SELECT w1, w2, w3, count(1) - 1 as `count` FROM n3gram_tmp GROUP BY w1, w2, w3 HAVING `count` > 0");
            System.out.println("Aggregate n4gram ...");
            stmt.executeUpdate("CREATE TABLE n4gram AS SELECT w1, w2, w3, w4, count(1) - 1 as `count` FROM n4gram_tmp GROUP BY w1, w2, w3, w4 HAVING `count` > 0");
            System.out.println("Aggregate n5gram ...");
            stmt.executeUpdate("CREATE TABLE n5gram AS SELECT w1, w2, w3, w4, w5, count(1) - 1 as `count` FROM n5gram_tmp GROUP BY w1, w2, w3, w4, w5 HAVING `count` > 0");
            System.out.println("Aggregate n6gram ...");
            stmt.executeUpdate("CREATE TABLE n6gram AS SELECT w1, w2, w3, w4, w5, w6, count(1) - 1 as `count` FROM n6gram_tmp GROUP BY w1, w2, w3, w4, w5, w6 HAVING `count` > 0");
            System.out.println("Aggregate person ...");
            stmt.executeUpdate("CREATE TABLE person AS SELECT name, count(1) - 1 as `count` FROM person_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate number ...");
            stmt.executeUpdate("CREATE TABLE number AS SELECT name, count(1) - 1 as `count` FROM number_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate organization ...");
            stmt.executeUpdate("CREATE TABLE organization AS SELECT name, count(1) - 1 as `count` FROM organization_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate location ...");
            stmt.executeUpdate("CREATE TABLE location AS SELECT name, count(1) - 1 as `count` FROM location_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate time ...");
            stmt.executeUpdate("CREATE TABLE time AS SELECT name, count(1) - 1 as `count` FROM time_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate date ...");
            stmt.executeUpdate("CREATE TABLE date AS SELECT name, count(1) - 1 as `count` FROM date_tmp GROUP BY name HAVING `count` > 0");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void createIndices(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Create index on n1gram");
            stmt.executeUpdate("CREATE INDEX n1gram_ix0 ON n1gram_tmp(w1)");
            System.out.println("Create index on n2gram");
            stmt.executeUpdate("CREATE INDEX n2gram_ix0 ON n2gram_tmp(w1, w2)");
            System.out.println("Create index on n3gram");
            stmt.executeUpdate("CREATE INDEX n3gram_ix0 ON n3gram_tmp(w1, w2, w3)");
            System.out.println("Create index on n4gram");
            stmt.executeUpdate("CREATE INDEX n4gram_ix0 ON n4gram_tmp(w1, w2, w3, w4)");
            System.out.println("Create index on n5gram");
            stmt.executeUpdate("CREATE INDEX n5gram_ix0 ON n5gram_tmp(w1, w2, w3, w4, w5)");
            System.out.println("Create index on n6gram");
            stmt.executeUpdate("CREATE INDEX n6gram_ix0 ON n6gram_tmp(w1, w2, w3, w4, w5, w6)");
            System.out.println("Create index on person");
            stmt.executeUpdate("CREATE INDEX person_ix0 ON person_tmp(name)");
            System.out.println("Create index on location");
            stmt.executeUpdate("CREATE INDEX location_ix0 ON location_tmp(name)");
            System.out.println("Create index on organization");
            stmt.executeUpdate("CREATE INDEX organization_ix0 ON organization_tmp(name)");
            System.out.println("Create index on time");
            stmt.executeUpdate("CREATE INDEX time_ix0 ON time_tmp(name)");
            System.out.println("Create index on date");
            stmt.executeUpdate("CREATE INDEX date_ix0 ON date_tmp(name)");
            System.out.println("Create index on number");
            stmt.executeUpdate("CREATE INDEX number_ix0 ON number_tmp(name)");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void storeNgreams(Connection conn, ArrayList<String> outToks)
            throws SQLException
    {
        String[] ngrams = new String[MAX_NGRAM];
        for (String tok : outToks) {
            for (int i = 0; i < MAX_NGRAM - 1; i++) {
                ngrams[i] = ngrams[i + 1];
            }
            ngrams[MAX_NGRAM - 1] = tok;
            updateN1Gram(conn, ngrams);
            updateN2Gram(conn, ngrams);
            updateN3Gram(conn, ngrams);
            updateN4Gram(conn, ngrams);
            updateN5Gram(conn, ngrams);
            updateN6Gram(conn, ngrams);
        }
    }

    private static void updateN1Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 1];
        if ("#b".equals(w1) || "#e".equals(w1))
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n1gram_tmp(w1) values(?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN2Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 2];
        String w2 = ngrams[MAX_NGRAM - 1];
        if (w1 == null || w2 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n2gram_tmp(w1, w2) values(?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN3Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 3];
        String w2 = ngrams[MAX_NGRAM - 2];
        String w3 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n3gram_tmp(w1, w2, w3) values(?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN4Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 4];
        String w2 = ngrams[MAX_NGRAM - 3];
        String w3 = ngrams[MAX_NGRAM - 2];
        String w4 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n4gram_tmp(w1, w2, w3, w4) values(?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN5Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 5];
        String w2 = ngrams[MAX_NGRAM - 4];
        String w3 = ngrams[MAX_NGRAM - 3];
        String w4 = ngrams[MAX_NGRAM - 2];
        String w5 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null || w5 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n5gram_tmp(w1, w2, w3, w4, w5) values(?, ?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.setString(5, w5);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN6Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 6];
        String w2 = ngrams[MAX_NGRAM - 5];
        String w3 = ngrams[MAX_NGRAM - 4];
        String w4 = ngrams[MAX_NGRAM - 3];
        String w5 = ngrams[MAX_NGRAM - 2];
        String w6 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null || w5 == null || w6 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n6gram_tmp(w1, w2, w3, w4, w5, w6) values(?, ?, ?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.setString(5, w5);
            stmt.setString(6, w6);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static ArrayList<String> addToken(ArrayList<String> tokens, String token) {
        if ((token.length() > 0) && (tokens.isEmpty() || !token.equals(tokens.get(tokens.size() - 1)))) {
            tokens.add(token);
        }
        return tokens;
    }

    private static ArrayList<String> getPosList(Connection conn, String pos, int count) throws SQLException {
        ArrayList<String> retVal = new ArrayList<String>();
        PreparedStatement stmt = null;
        try {
            String sql = "select stem from vocabulary where posTag like '" + pos + "%' and count > 1 order by count desc limit ?";
            stmt = conn.prepareStatement(sql);
            stmt.setInt(1, count);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                retVal.add(rs.getString("stem"));
            }
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return retVal;
    }

    public static HashSet<String> buildVocabulary(String fileName, int nounCount, int verbCount, int adjCount) throws IOException {
        HashSet<String> retVal = new HashSet<String>();
        Connection conn = null;
        try {
            conn = openDatabase(fileName);
            retVal.addAll(getPosList(conn, "N", nounCount));
            retVal.addAll(getPosList(conn, "V", verbCount));
            retVal.addAll(getPosList(conn, "J", adjCount));
            retVal.add("m");
            retVal.add("d");
            retVal.add("re");
            retVal.add("ve");
        } catch (SQLException e) {
            throw new IOException(e.toString());
        } finally {
            if (conn != null) { try { conn.close(); } catch (Exception e) { } }
        }
        return retVal;
    }

    private static Connection openDatabase(String fileName) throws SQLException {
        File outputFile = new File(fileName);
        String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
        Connection conn = DriverManager.getConnection(url);
        return conn;
    }

    private static Connection initializeDatabase(String outputFileName) throws SQLException {
        File outputFile = new File(outputFileName);
        if (outputFile.exists()) {
            outputFile.renameTo(new File(outputFileName + "." + System.currentTimeMillis()));
        }
        Connection conn = null;
        Statement stmt = null;
        String sql;
        try {
            String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
            conn = DriverManager.getConnection(url);
            conn.setAutoCommit(false);

            stmt = conn.createStatement();
            sql = "CREATE TABLE n1gram_tmp(" +
                    " w1        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n2gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n3gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n4gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n5gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n6gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL," +
                    " w6        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE person_tmp(name              TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE organization_tmp(name        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE time_tmp(name                TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE date_tmp(name                TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE location_tmp(name            TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE number_tmp(name              TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return conn;
    }
}
