package make_corpus;

import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.stemmer.PorterStemmer;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.util.Span;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Created by serge on 5/10/2017.
 */
public class GatherStats {
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
        InputStream sentenceModelModelIn = null;
        InputStream tokenNameFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        InputStream posModelIn = null;
        try {
            Models models = new Models();
            sentenceModelModelIn = new FileInputStream("en-sent.bin");
            models.sentenceModel = new SentenceModel(sentenceModelModelIn);
            tokenizerModelIn = new FileInputStream("en-token.bin");
            models.tokenizerModel = new TokenizerModel(tokenizerModelIn);
            tokenNameFinderModelIn = new FileInputStream("en-ner-person.bin");
            models.tokenNameFinderModel = new TokenNameFinderModel(tokenNameFinderModelIn);
            posModelIn = new FileInputStream("en-pos-maxent.bin");
            models.posModel = new POSModel(posModelIn);

            System.out.println("../src_data/en_US/en_US.blogs.txt");
            processSingleFile(models, "../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.stats.db");
            System.out.println("../src_data/en_US/en_US.news.txt");
            processSingleFile(models, "../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.stats.db");
            System.out.println("../src_data/en_US/en_US.twitter.txt");
            processSingleFile(models, "../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.stats.db");
        } finally {
            if (posModelIn != null) {
                try {
                    posModelIn.close();
                } catch (Exception e) {
                }
            }
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
        PorterStemmer stemmer = new PorterStemmer();
        BufferedReader reader = null;
        Connection conn = null;
        HashMap< Pair<String, String>, Integer> stats = new HashMap<Pair<String, String>, Integer>();
        try {
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(models.sentenceModel);
            Tokenizer tokenizer = new TokenizerME(models.tokenizerModel);
            POSTaggerME tagger = new POSTaggerME(models.posModel);

            reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFileName)));
            int count = 0;
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences = sentenceDetector.sentDetect(line);
                for (String rawSentence : sentences) {
                    System.out.printf("\rSentence : %d", ++count);
                    rawSentence = replaceAll(rawSentence, "\u201c", "\"");
                    rawSentence = replaceAll(rawSentence, "\u201d", "\"");
                    rawSentence = replaceAll(rawSentence, "\u2019", "\'");
                    rawSentence = replaceAll(rawSentence, "wanna", "want to");
                    rawSentence = replaceAll(rawSentence, "gonna", "going to");

                    rawSentence = rawSentence.replaceAll("[^\\p{ASCII}]", "");

                    String[] sentence = tokenizer.tokenize(rawSentence);
                    String postags[] = tagger.tag(sentence);

                    ArrayList<String> outToks = new ArrayList<String>();
                    int sparseCount = 0, nonSparseCount = 0;
                    for (int i = 0; i < sentence.length; i++) {
                        String token = sentence[i].replaceAll("\\p{Punct}", "");
                        token = token.replaceAll("\\p{Digit}", "");
                        if (token.length() == 0)
                            continue;

                        String posTag = postags[i];

                        if ("s".equalsIgnoreCase(token)) {
                            if ("VBZ".equals(posTag)) {
                                token = "is";
                            } else if (!outToks.isEmpty()) {
                                int ix = outToks.size() - 1;
                                String oldToken = outToks.get(ix);
                                outToks.set(ix, oldToken + "'s");
                                continue; // We aren't going to introduce a new token here, so let's skip iteration.
                            }
                        } else if ("nt".equalsIgnoreCase(token) && "RB".equals(posTag)) {
                            token = "not";
                            if (!outToks.isEmpty()) {
                                int ix = outToks.size() - 1;
                                String oldToken = outToks.get(ix);
                                if ("ca".equalsIgnoreCase(oldToken)) {
                                    outToks.set(ix, "can");
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
                        } else if ("d".equalsIgnoreCase(token) && "MD".equals(posTag)) {
                            token = "would";
                        } else if ("u".equalsIgnoreCase(token) && "PRP".equals(posTag)) {
                            token = "you";
                        }
                        if (posTag.startsWith("N")
                                || posTag.startsWith("V")
                                || posTag.startsWith("J")
                                || posTag.startsWith("R")
                                || posTag.startsWith("M")
                                || posTag.startsWith("I")
                                || posTag.startsWith("P")
                                || posTag.startsWith("W"))
                        {
                            Pair<String, String> key = new ImmutablePair<String, String>(stemmer.stem(token.toLowerCase()), posTag);
                            int counter = stats.containsKey(key) ? stats.get(key) : 0;
                            stats.put(key, ++counter);
                        }
                    }
                }
            }
            conn = initializeDatabase(outputFileName);
            try {
                conn.setAutoCommit(false);
                Set<Pair<String, String>> keySet = stats.keySet();
                int c = 0, cUb = keySet.size();
                for (Pair<String, String> key : keySet) {
                    System.out.printf("\rInserting : %d / %d", ++c, cUb);
                    int counter = stats.get(key);
                    insertNewValue(conn, key.getLeft(), key.getRight(), counter);
                }
                conn.commit();
            } catch (Exception e) {
                conn.rollback();
                throw new RuntimeException(e);
            }
            System.out.print("\rCreating index...");
            conn.setAutoCommit(true);
            createIndex(conn);
            System.out.print("\rDONE\n");
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
        } catch (SQLException e) {
            throw new IOException(e.toString());
        } finally {
            if (conn != null) { try { conn.close(); } catch (Exception e) { } }
        }
        return retVal;
    }

    private static void insertNewValue(Connection conn, String stem, String posTag, int counter) throws SQLException {
        PreparedStatement stmt = null;
        try {
            String sql = "INSERT INTO vocabulary(stem, posTag, count) VALUES(?,?,?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, stem);
            stmt.setString(2, posTag);
            stmt.setInt(3, counter);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void createIndex(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            String sql = "CREATE INDEX vocabulary_ix ON vocabulary (posTag, count)";
            stmt.executeUpdate(sql);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
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
        try {
            String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
            conn = DriverManager.getConnection(url);
            stmt = conn.createStatement();
            String sql = "CREATE TABLE vocabulary " +
                    "(stem        TEXT  NOT NULL," +
                    " posTag      TEXT  NOT NULL, " +
                    " count INT   NOT NULL)";
            stmt.executeUpdate(sql);
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return conn;
    }
}
