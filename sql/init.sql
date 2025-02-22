CREATE USER my_user WITH PASSWORD 'my_password';
Create DATABASE my_db;
GRANT ALL PRIVILEGES ON DATABASE my_db TO my_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO my_user;


CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    login VARCHAR NOT NULL,
    pass VARCHAR NOT NULL
);

CREATE TABLE sessions (
    id VARCHAR PRIMARY KEY,
    user_id BIGINT NOT NULL,
    active_to TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);


CREATE TABLE chats (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR NOT NULL,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    creator_id BIGINT NOT NULL,
    FOREIGN KEY (creator_id) REFERENCES users (id) ON DELETE CASCADE
);


CREATE TABLE chat_messages (
    id BIGSERIAL PRIMARY KEY,
    chat_id BIGINT NOT NULL,
    message VARCHAR NOT NULL,
    reply_for BIGINT,
    author_id BIGINT NOT NULL,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    edited_at TIMESTAMP WITHOUT TIME ZONE,
    FOREIGN KEY (chat_id) REFERENCES chats (id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (reply_for) REFERENCES chat_messages (id) ON DELETE SET NULL
);

CREATE TABLE chat_members (
    chat_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    is_owner BOOLEAN NOT NULL,
    PRIMARY KEY (chat_id, user_id),
    FOREIGN KEY (chat_id) REFERENCES chats (id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);

CREATE TABLE last_read_msg_in_chat (
    chat_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    chat_message_id BIGINT NOT NULL,
    PRIMARY KEY (chat_id, user_id),
    FOREIGN KEY (chat_id) REFERENCES chats (id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (chat_message_id) REFERENCES chat_messages (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS nodes (
    node_id text PRIMARY KEY,
    host text NOT NULL,
    web_port integer NOT NULL,
    last_seen timestamp NOT NULL,
    user_sessions jsonb DEFAULT '[]'
);