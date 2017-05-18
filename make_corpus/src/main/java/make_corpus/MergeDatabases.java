package make_corpus;

import java.io.File;
import java.sql.*;

/**
 * Created by serge on 5/17/2017.
 */
public class MergeDatabases {
    static {
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /*         try {
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
                    threadMain("../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.stats.db");*/
    public static void main(String[] argv) throws SQLException {
        File tmpFile = null;
        Connection tmpConn = null;
        Connection destConn = null;
        try {
            tmpFile = new File("../src_data/en_US/tmp" + System.currentTimeMillis() + ".db");
            tmpConn = initializeTmpDatabase(tmpFile);
            gatherAllRecords("../src_data/en_US/en_US.blogs.stats.db", tmpConn);
            gatherAllRecords("../src_data/en_US/en_US.news.stats.db", tmpConn);
            gatherAllRecords("../src_data/en_US/en_US.twitter.stats.db", tmpConn);
        } finally {
            if (tmpConn != null) {
                try {
                    tmpConn.close();
                } catch (Exception e) {
                    // swallow if any
                }
            }
            if (destConn != null) {
                try {
                    destConn.close();
                } catch (Exception e) {
                    // swallow if any
                }
            }
            /*if (tmpFile != null && tmpFile.exists()) {
                try {
                    tmpFile.delete();
                } catch (Exception e) {
                    // swallow if any
                }
            }*/
        }
    }

    private static void gatherAllRecords(String fileName, Connection tmpConn) throws SQLException {
        Connection srcConn = null;
        try {
            System.out.printf("Source: %s\n", fileName);
            srcConn = openDatabase(fileName);
            copyNgrams(srcConn, tmpConn);
            srcConn.commit();
        } finally {
            if (srcConn != null) {
                try {
                    srcConn.close();
                } catch (Exception e) {
                    // swallow if any
                }
            }
        }
    }

    private static void copyNgrams(Connection srcConn, Connection tmpConn) throws SQLException {
        for (int i = 1; i <= GatherStats.MAX_NGRAM; i++) {
            System.out.printf("n%dgrams\n", i);
            PreparedStatement stmtRead = null, stmtWrite = null;
            ResultSet rs = null;
            try {
                stmtRead = srcConn.prepareStatement("select count(*) as `totalCount` from n" + i + "gram");
                int totalCount = 1;
                rs = stmtRead.executeQuery();
                if (rs.next()) {
                    totalCount = rs.getInt("totalCount");
                }
                stmtRead.close();
                rs.close();
                stmtWrite = srcConn.prepareStatement(copyNgrams_composeInsertStatement(i));
                int c = 0;
                stmtRead = srcConn.prepareStatement("select * from n" + i + "gram");
                rs = stmtRead.executeQuery();
                while (rs.next()) {
                    System.out.printf("\r%d", (int)(((double)++c) / ((double)totalCount) * 100. ));
                    int j;
                    for (j = 1; j <= i; j++) {
                        String w =  rs.getString("w" + j);
                        stmtWrite.setString(j, w);
                    }
                    int count = rs.getInt("count");
                    stmtWrite.setInt(j, count);
                    stmtWrite.execute();
                }
            } finally {
                if (stmtRead != null) {
                    try {
                        stmtRead.close();
                    } catch (Exception e) {
                        // swallow if any
                    }
                }
                if (stmtWrite != null) {
                    try {
                        stmtWrite.close();
                    } catch (Exception e) {
                        // swallow if any
                    }
                }
            }
        }
    }

    private static String copyNgrams_composeInsertStatement(int ngram) {
        StringBuilder sb = new StringBuilder();
        sb.append("insert into n").append(ngram).append("gram(");
        for (int i = 1; i <= ngram; i++) {
            if (i != 1) {
                sb.append(",");
            }
            sb.append("w").append(i);
        }
        sb.append(", `count`) values(");
        for (int i = 1; i <= ngram; i++) {
            if (i != 1) {
                sb.append(",");
            }
            sb.append("?");
        }
        sb.append(", ?)");
        return sb.toString();
    }

    private static Connection openDatabase(String fileName) throws SQLException {
        File outputFile = new File(fileName);
        String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
        Connection conn = DriverManager.getConnection(url);
        conn.setAutoCommit(false);
        return conn;
    }

    private static Connection initializeTmpDatabase(File outputFile) throws SQLException {
        Connection conn = null;
        Statement stmt = null;
        String sql;
        try {
            String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
            conn = DriverManager.getConnection(url);
            conn.setAutoCommit(false);

            stmt = conn.createStatement();
            sql = "CREATE TABLE n1gram(" +
                    " w1        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n2gram(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n3gram(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n4gram(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n5gram(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n6gram(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL," +
                    " w6        TEXT  NOT NULL," +
                    "`count`    INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE person(name TEXT NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE organization(name TEXT  NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE time(name TEXT NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE date(name TEXT NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE location(name TEXT NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE number(name TEXT NOT NULL, `count` INTEGER)";
            stmt.executeUpdate(sql);

            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return conn;
    }
}
