CONTEXT
=======

Tulip Factory frequently runs 24/7, either looking at visualizations or when acting as a player. This constant usage means that memory leaks are serious issues, and we've built some tools to track them down.

One time, when dealing with a hard-to-find memory leak, we added some code to our website that would make perodic requests to a webserver with memory usage, the current URL, and if the page had crashed. We've given you some example data from this, and we’d like you to create a web app that shows memory usage and crashes for chrome. You can use whatever technology you're comfortable with.

REQUIREMENTS
============

You must create a web app that provides a UI for exploring the data. It should help the user find relationships between pages and crashes, frequency of crashes, or any other interesting trends in the data. You can use graphs, charts, animations, timelines, maps, etc. It should have some sort of interactivity. You're welcome to use any third-party libraries you'd like for this.

We've given you 20,000 sample reports from a single machine. You may connect to the Postgres database, or you can get the data from the JSON file.

The sample data has 4 fields:

1. timestamp. Integer. Seconds since epoch time that the report was recorded.

2. bytes_used. Integer. The number of bytes that the webpage was using.

3. current_page. String. The current relative URL that the webpage was on.

4. did_aww_snap. Boolean. If the page crashed since the last report.

DELIVERABLES
============

1. Source code and instructions for getting it running, committed to the private git repo we've set up for you.

RESOURCES
=========

## Postgres Database

We've created a database containing 20,000 datapoints. This database is read-only. The schema for the database is:

```sql
CREATE TABLE reports (
    id SERIAL,
    timestamp integer NOT NULL,
    bytes_used integer NOT NULL,
    current_page varchar(255) NOT NULL,
    did_aww_snap boolean NOT NULL
);

CREATE INDEX reports_timestamp_index ON reports
    USING btree (timestamp);

CREATE INDEX reports_bytes_used_index ON reports
    USING btree (bytes_used);

CREATE INDEX reports_current_page_index ON reports
    USING btree (current_page);

CREATE INDEX reports_did_aww_snap_index ON reports
    USING btree (did_aww_snap);
```

You can connect to the database with the following parameters:

* **host**: `aws-us-east-1-portal.8.dblayer.com`

* **user**: `candidate`

* **password**: `giap-quib-fac-wav-mi`

* **database**: `memory_tracker`

* **port**: `10131`

## JSON

We've also put the sample data into a JSON file. The file contains an array with 20,000 objects, each one with 4 keys (`timestamp`, `bytes_used`, `current_page`, `did_aww_snap`).
