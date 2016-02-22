CREATE TABLE reports (id SERIAL, timestamp integer NOT NULL, bytes_used integer NOT NULL, current_page character varying(255) NOT NULL, did_aww_snap boolean NOT NULL);

CREATE INDEX reports_timestamp_index ON reports USING btree (timestamp);

CREATE INDEX reports_bytes_used_index ON reports USING btree (bytes_used);

CREATE INDEX reports_current_page_index ON reports USING btree (current_page);

CREATE INDEX reports_did_aww_snap_index ON reports USING btree (did_aww_snap);

GRANT SELECT ON ALL TABLES IN schema public to canidate;
