--Verify the load was succesful 
-- check the rows count
SELECT count(*) num_rows
  FROM GOP_Debate;
  
-- check column count
 SELECT  NAME, CREATOR, CTIME, COLCOUNT
  FROM  SYSIBM.SYSTABLES
  WHERE  CREATOR ='MFS65449'; 
  
-- output nulls per column
SELECT  NAME, TBNAME, TBCREATOR, COLTYPE, NULLS, LENGTH
  FROM  SYSIBM.SYSCOLUMNS 
 WHERE  TBCREATOR='MFS65449'
   AND  TBNAME='GOP_DEBATE';

-- Begin exploring the data

 -- how many times is each candidate tweeted about
  SELECT  CANDIDATE, count(CANDIDATE) "Times Mentioned"
  FROM GOP_Debate
 GROUP BY CANDIDATE;

--Counts of candidate and sentiment

SELECT  CANDIDATE, 
                   sum(case when SENTIMENT='Negative' 
                                then 1 
                                else 0 
                        end) "Negative",
                 sum(case when SENTIMENT='Positive' 
                               then 1 
                               else 0 end
                         ) "Positive",
                sum(case when SENTIMENT='Neutral'
                                then 1 
                                else 0 end
                        ) "Neutral",
                count(CANDIDATE) "Total"
     FROM GOP_Debate
GROUP BY  CANDIDATE
ORDER BY upper(CANDIDATE) ;

-- candidates with above average number of positive tweets   
 SELECT  CANDIDATE, count(CANDIDATE) "Positive Tweets"
 FROM GOP_Debate
WHERE  SENTIMENT='Positive'
GROUP BY CANDIDATE
    HAVING count(CANDIDATE) >
             ( SELECT avg(positive_count) 
                  FROM (
                         SELECT CANDIDATE, count(CANDIDATE)  positive_count
                           FROM GOP_Debate
                           WHERE  SENTIMENT='Positive'
                                       GROUP BY CANDIDATE
                             )
             )
             ORDER BY "Positive Tweets" DESC;
 
-- amount above average number of positive tweets   
 SELECT  CANDIDATE, count(CANDIDATE) -   ( SELECT avg(positive_count) 
                  FROM (
                         SELECT CANDIDATE, count(CANDIDATE)  positive_count
                           FROM GOP_Debate
                           WHERE  SENTIMENT='Positive'
                                       GROUP BY CANDIDATE
                             )
             ) "Positive Tweets above Avg"
 FROM GOP_Debate
WHERE  SENTIMENT='Positive'
GROUP BY CANDIDATE
ORDER BY "Positive Tweets above Avg" DESC;

-- number above average number of negative tweets   
 SELECT  CANDIDATE, count(CANDIDATE) -   ( SELECT avg(negative_count) 
                  FROM (
                         SELECT CANDIDATE, count(CANDIDATE)  negative_count
                           FROM GOP_Debate
                           WHERE  SENTIMENT='Negative'
                                       GROUP BY CANDIDATE
                             )
             ) "Amount of Negative Tweets above Avg"
 FROM GOP_Debate
WHERE  SENTIMENT='Negative'
GROUP BY CANDIDATE
ORDER BY "Amount of Negative Tweets above Avg" DESC;

-- tweets by time zone
SELECT CANDIDATE,  USER_TIMEZONE, COUNT(CANDIDATE) "Number of Tweets"
FROM GOP_Debate
GROUP BY CANDIDATE, USER_TIMEZONE
ORDER BY CANDIDATE;

-- candidates with above avg number of tweets in usa 
SELECT  CANDIDATE, count(CANDIDATE) "Times Mentioned"
 FROM GOP_Debate
 WHERE USER_TIMEZONE IN ('Alaska', 'America/Chicago', 'America/Denver', 'America/New_York', 'America/Los_Angeles', 'Arizona', 'Central Time (US & Canada)', 'Eastern Time (US & Canada)', 'Hawaii', 'Mountain Time (US & Canada)', 'Pacific Time (US & Canada)', 'America/Phoenix', 'EST', 'America/Detroit', 'EDT', 'Indiana (East)' )
AND CANDIDATE != 'No candidate mentioned'
GROUP BY CANDIDATE
HAVING count(CANDIDATE) > 
                           ( SELECT avg(NUM_count) 
                  FROM (
                         SELECT CANDIDATE, count(CANDIDATE)  NUM_count
                       FROM GOP_Debate
                       WHERE  USER_TIMEZONE IN ('Alaska', 'America/Chicago', 'America/Denver', 'America/New_York', 'America/Los_Angeles', 'Arizona', 'Central Time (US & Canada)', 'Eastern Time (US & Canada)', 'Hawaii', 'Mountain Time (US & Canada)', 'Pacific Time (US & Canada)', 'America/Phoenix', 'EST', 'America/Detroit', 'EDT', 'Indiana (East)' )
                       AND CANDIDATE != 'No candidate mentioned'
                  GROUP BY CANDIDATE
                             )
             );

--    number ot times candidate mentioned by popular tweeter
SELECT  CANDIDATE, count(CANDIDATE) "Times Mentioned by above Avg retweeter"
 FROM GOP_Debate 
 WHERE   RETWEET_COUNT >  
                                        (SELECT avg(RETWEET_COUNT) 
                                        FROM GOP_Debate
                                       )
 GROUP BY CANDIDATE
 ORDER BY "Times Mentioned by above Avg retweeter" DESC;
 ;

 -- number of tweeters per candidate
 SELECT CANDIDATE,   COUNT(CANDIDATE) "Number of Tweeters"
  FROM (
                     SELECT CANDIDATE, NAME  
                        FROM GOP_Debate
                   WHERE CANDIDATE != 'No candidate mentioned'
               GROUP BY CANDIDATE, NAME
 
        )
 WHERE CANDIDATE != 'No candidate mentioned'
GROUP BY CANDIDATE
ORDER BY "Number of Tweeters" DESC;

