SELECT count(*) FROM trk_staging_trials;

SELECT count(*) FROM trk_trials;

SELECT count(*) FROM trials_view;

SELECT count(*) FROM review_view;

SELECT count(*) FROM combined_view;

SELECT created_at, count(created_at) FROM trk_trials
group by created_at
order by created_at asc;

SELECT study_design_final, count(study_design_final) FROM trials_view
group by study_design_final;