{{ config(materialized='table') }}

WITH player_data AS (
    SELECT * FROM {{ ref('stg_player') }}
),

latest_player_info AS (
    SELECT
        player_id,
        player_name,
        player_short_name,
        player_position,
        player_position_abbr,
        player_image,
        team_name,
        ROW_NUMBER() OVER (
            PARTITION BY player_id 
            ORDER BY game_date DESC
        ) AS row_num
    FROM player_data
),

unique_players AS (
    SELECT
        player_id,
        player_name,
        player_short_name,
        player_position,
        player_position_abbr,
        player_image,
        team_name,
    FROM latest_player_info
    WHERE row_num = 1
),

player_stats AS (
    SELECT
        player_id,
        -- Game counts
        COUNT(DISTINCT game_id) AS games_played,

        -- Career averages
        ROUND(AVG(points), 1) AS avg_points,
        ROUND(AVG(rebounds), 1) AS avg_rebounds,
        ROUND(AVG(assists), 1) AS avg_assists,
        ROUND(AVG(steals), 1) AS avg_steals,
        ROUND(AVG(blocks), 1) AS avg_blocks,
        ROUND(AVG(minutes), 1) AS avg_minutes,
        
        -- Career shooting percentages
        ROUND(SUM(field_goals_made) / NULLIF(SUM(field_goals_attempted), 0) * 100, 1) AS career_fg_pct,
        ROUND(SUM(three_point_field_goals_made) / NULLIF(SUM(three_point_field_goals_attempted), 0) * 100, 1) AS career_3pt_pct,
        ROUND(SUM(free_throws_made) / NULLIF(SUM(free_throws_attempted), 0) * 100, 1) AS career_ft_pct,
        
        -- Career totals
        SUM(points) AS total_points,
        SUM(rebounds) AS total_rebounds,
        SUM(assists) AS total_assists,
        SUM(steals) AS total_steals,
        SUM(blocks) AS total_blocks,
        SUM(minutes) AS total_minutes,
        
    FROM player_data
    WHERE did_not_play = FALSE
    GROUP BY player_id
)

SELECT
    p.player_name,
    p.player_short_name,
    p.player_position,
    p.player_image,
    p.team_name,
    s.games_played,
    s.avg_points,
    s.avg_rebounds,
    s.avg_assists,
    s.avg_steals,
    s.avg_blocks,
    s.avg_minutes,
    s.career_fg_pct,
    s.career_3pt_pct,
    s.career_ft_pct,
    s.total_points,
    s.total_rebounds,
    s.total_assists,
    s.total_steals,
    s.total_blocks,
    s.total_minutes,
    CURRENT_TIMESTAMP() AS updated_at
FROM unique_players p
JOIN player_stats s ON p.player_id = s.player_id
