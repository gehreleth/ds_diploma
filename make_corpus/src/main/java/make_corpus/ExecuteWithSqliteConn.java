package make_corpus;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * Created by serge on 5/18/2017.
 */
public interface ExecuteWithSqliteConn {
    void run(Connection conn) throws SQLException;
}
