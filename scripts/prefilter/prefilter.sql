-- Pre-filtering script for trials
--
-- Requirement to filter in
-- Type of trial:
-- All RCTs with control arm (placebo or active control)
-- Phases of Trials (spoke to the Vancouver Team, and this will be added):
-- - Phase 2 trials n>80
-- - All other trials, n>200
-- Categories wrt Clinical setting, ie COVID19 patient severity:

-- Indexing 
-- DROP INDEX IF EXISTS idx_trk_trials_phase;
-- CREATE INDEX idx_trk_trials_phase ON trk_trials(phase);

-- Orientation

-- The latest trial summaries are in `trials_view`

-- SELECT COUNT(1) FROM trials_view;                        -- As of 26/Aug yields 3590
-- SELECT COUNT(DISTINCT trial_id) FROM trials_view;        -- 2105
-- SELECT DISTINCT(patient_setting) from trials_view;       -- 31 
-- SELECT DISTINCT phase from trk_trials ORDER BY phase;    -- 36

-- Only select from trials view (most recent records for each trial)
WITH ts AS (
    SELECT
    CASE WHEN phase IN ('2', 
                        'II', 
                        'Ii', 
                        'Iib', 
                        'Phase-2',
                        'Human Pharmacology (I): No Therapeutic Exploratory (Ii): Yes Therapeutic Confirmatory - (Iii): No Therapeutic Use (Iv): No') 
                THEN '2'
                ELSE 'Other' END AS phase_
    , CASE WHEN 
            LOWER(tx1_category) LIKE '%soc%' 
            OR LOWER(tx2_category) LIKE '%placebo%' 
            OR LOWER(tx2_category) LIKE '%soc%' 
            OR LOWER(tx3_category) LIKE '%placebo%' 
            OR LOWER(tx3_category) LIKE '%soc%' 
            OR LOWER(tx4_category) LIKE '%placebo%' 
            OR LOWER(tx4_category) LIKE '%soc%' 
            OR LOWER(tx5_category) LIKE '%placebo%' 
            OR LOWER(tx5_category) LIKE '%soc%' 
        THEN 1 ELSE 0 END AS has_control_arm
    , *
FROM
    trials_view
)
INSERT INTO trk_reviews (trial_id, flag, user_submitted, note, rating)
SELECT 
    trial_id
    , TRUE AS flag 
    , 'prefilter' AS user_submitted 
    , 'Prefilter run ' || CURRENT_TIMESTAMP AS note
    , 75 AS rating
FROM 
    ts
WHERE 
-- Control Arm
has_control_arm = 1
-- Enrollment criteria
AND
(
    (phase_ = '2' and expected_enrollment >= 80)
    OR
    (phase_ != '2' and expected_enrollment >= 200)
)
-- Clinical severity categories
AND
    patient_setting IN ('Hospital', 'ICU', 'Hospital, ICU')
;


-- Look at the effects 
-- The table `trk_reviews` holds all the reviewa
-- The *view* `review_view` holds the most recent review for a given `trial_id`

-- SELECT COUNT(1) FROM trk_reviews WHERE user_submitted = 'prefilter';
-- SELECT * FROM trk_reviews LIMIT 10;
-- SELECT * FROM review_view LIMIT 10;

-- Housekeeping - run this to clear all the automated reviews
-- DELETE FROM trk_reviews WHERE user_submitted = 'prefilter';    

