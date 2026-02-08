# 8086-Retro-PingPong-Ball-game
8086 Assembly Ping Pong A fully functional, feature-rich Ping Pong game written in 8086 Assembly Language using EMU8086. This project demonstrates low-level hardware interaction, BIOS interrupt handling, and real-time game logic.

Features

Dynamic Difficulty: Choose between Easy, Medium, and Hard modes which adjust both ball and paddle speeds.

Multi-Ball Chaos: A second ball (Multi-Ball) activates automatically once a player reaches 3 points, increasing the challenge.

Color Graphics: Utilizes BIOS INT 10h to render colorful paddles, balls, and UI elements.

Countdown System: A synchronized 3-2-1-GO! countdown with audio beeps before the match starts.

Score Tracking: Real-time score rendering at the top of the screen with a win condition of 5 points.

Audio Feedback: PC speaker beeps for wall bounces, paddle hits, and scoring.

Controls Player A (Left): W to move up, S to move down. Player B (Right): I to move up, K to move down. Restart: Press R after a game ends. Exit: Press any other key after a game ends.

Technical Implementation This project interacts directly with the CPU and BIOS:

Graphics: Uses INT 10h (Video Services) for cursor positioning and character rendering with color attributes.

Timing: Uses INT 15h, AH=86h for precise microsecond delays to manage game speed and countdowns.

Input: Uses INT 16h for non-blocking keyboard polling to ensure smooth gameplay.

Sound: Uses ASCII 07h (BEL) via INT 21h for simple audio feedback.
