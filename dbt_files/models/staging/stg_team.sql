{{ config(
    materialized='table',
    partition_by={
      "field": "game_date",
      "data_type": "date",
      "granularity": "day"
    },
    cluster_by = "team_id"
)}}

WITH team_game_data AS (
    select
    -- Primary keys
        team_id,
        game_id,
        
    -- Dates
        CAST(game_date AS DATE) AS game_date,

    -- Team info
        team_name,
        team_location,
        team_abbreviation,
        team_display_name AS team_full_name,
        team_short_display_name,
        team_uid,
        team_slug,
        team_color,
        team_alternate_color,
        team_logo,
    -- Game context
        season,
        season_type,
        team_home_away AS home_away,
        team_score,
        team_winner AS is_winner,

    -- Team stats
        assists,
        blocks,
        defensive_rebounds,
        fast_break_points,
        field_goal_pct,
        field_goals_made,
        field_goals_attempted,
        flagrant_fouls,
        fouls,
        free_throw_pct,
        free_throws_made,
        free_throws_attempted,
        largest_lead,
        offensive_rebounds,
        points_in_paint,
        steals,
        team_turnovers,
        technical_fouls,
        three_point_field_goal_pct,
        three_point_field_goals_made,
        three_point_field_goals_attempted,
        total_rebounds,
        total_technical_fouls,
        total_turnovers,
        turnover_points,
        turnovers,
        ROW_NUMBER() OVER (
            PARTITION BY team_id, game_id 
            ORDER BY game_date DESC  -- Assuming you want the most recent record in case of duplicates
            ) AS row_num
        from {{ source('team_db', 'team_table') }}
)

SELECT 
    * EXCEPT(row_num) 
FROM 
    team_game_data 
WHERE 
    row_num = 1