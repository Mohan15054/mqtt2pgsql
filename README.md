# mqtt2pgsql

EMQX Plugin push MQTT Publish message to PostgreSQL


## Build

Requirements:
- Erlang/OTP 25 [erts-13.2.2] 
- Rebar3

Build command

```bash
make
```

Build and Release command

```bash
make && make rel
```

Build Release output location

```bash
_build/default/emqx_plugrel/mqtt2pgsql-<vsn>.tar.gz
```

## Configuration Parameters

### 1. **host**
   - **Description:** Specifies the hostname or IP address of the PostgreSQL database server.
   - **Example:** `host = emqx-db`

### 2. **port**
   - **Description:** Specifies the port number on which the PostgreSQL server is listening.
   - **Example:** `port = 5432`

### 3. **username**
   - **Description:** Specifies the username used to connect to the PostgreSQL database.
   - **Example:** `username = postgres`

### 4. **password**
   - **Description:** Specifies the password used to authenticate the connection to the PostgreSQL database.
   - **Example:** `password = postgres`

### 5. **dbname**
   - **Description:** Specifies the name of the PostgreSQL database to which the connector will connect.
   - **Example:** `dbname = postgres`

### 6. **poolsize**
   - **Description:** Sets the maximum number of simultaneous database connections in the connection pool.
   - **Example:** `poolsize = 10`

### 7. **schemacount**
   - **Description:** `schemacount` specifies the nth element in the MQTT topic path to determine the schema name for storing the MQTT messages. 
   - **Example:** If `schemacount = 3` and the MQTT topic is `test/company/public/tablename`, the schema name would be `public` which is the `third element in the topic`.

### 8. **tablecount**
   - **Description:** `tablecount` specifies the nth element in the MQTT topic path to determine the table name for storing the MQTT messages. 
   - **Example:**  If `tablecount = 4` and the MQTT topic is `test/company/public/tablename` the table name would be derived from the `fourth element in the topic` which is `tablename`.

### 9. **tablepre**
   - **Description:** Specifies the prefix to be used for table names. Useful for avoiding naming conflicts.
   - **Example:** `tablepre = "\""`

### 10. **tablepost**
   - **Description:** Specifies the postfix to be used for table names. Useful for avoiding naming conflicts.
   - **Example:** `tablepost =  "\""`

### 11. **error_schema**
   - **Description:** Specifies the schema to store error records if there are issues during data processing.
   - **Example:** `error_schema = public`

### 12. **error_table**
   - **Description:** Specifies the table name to store error records if there are issues during data processing.
   - **Example:** `error_table = errors`

### Configuration loction
`mqtt2pgsql` expects the configuration at `/opt/emqx/etc/mqtt2pgsql.hocon`.

### Example Configuration file

File Name : `mqtt2pgsql.hocon`

File Location: `/opt/emqx/etc/`

File Content:

```hocon
mqtt2pgsql {
    host = emqx-db 
    port = 5432
    username = postgres
    password = postgres
    dbname = postgres
    poolsize = 10
    schemacount = 3
    tablecount = 4
    tablepre = "\""
    tablepost =  "\""
    error_schema = public 
    error_table = errors
}
```

