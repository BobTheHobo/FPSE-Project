## Overview:

This project is a web application hosting a 2D grid-based game where players must navigate from a starting point to an endpoint, avoiding cells that become dangerous through the progression of a “simulation” similar to Conway’s Game of Life.

The simulation operates with a custom rule set involving “ice” and “fire” cells, which interact with each other and influence neighboring cells. The simulation evolves with each player move, adding a layer of strategy as the player avoids dangerous cells to safely reach the endpoint.

We plan on implementing the A\* algorithm to determine if a path exists from the player’s current position to the endpoint. This pathfinding will consider the player’s starting position, the destination, and the positions of dangerous obstacles as they evolve through the simulation. If the algorithm determines that a path doesn’t exist from the player’s current position to the endpoint, the endpoint is moved to a new random reachable spot on the board.

## Mockups

#### Start Menu:

![][image1]

#### In-game Preview:

![][image2]

#### Game Over:

![][image3]

#### Winning:

![][image4]

## Libraries:

### Frontend:

* **Dream**: For handling the web server aspects of the application, such as hosting the interface and facilitating client-server communication.  
* **Lwt**: To manage asynchronous operations within the application, necessary for interactions within the simulation.  
* **ReScript**: To compile OCaml code to JavaScript, enabling frontend functionality and integrating with the backend simulation.

### Backend:

* **Core**: To handle backend logic, including simulation state management, rule-based cell interactions, and A\* pathfinding.

## Demo: 

#### Dream, Lwt, and ReScript:

- Build the project  
- CD into “backend/\_build/default/src/bin/  
- Run “./server.exe”  
- CD into “frontend/”  
- Run “npm run dev”  
- Navigate to the localhost link  
- Use arrowkeys to move the player (Yellow) to the goal (Green), avoiding the obstacles (Red)

#### Core:

- Build the project  
- CD into “backend/\_build/default/src/bin/  
- Run “./cli.exe”  
- Displayed is an oscillating conway pattern  
- Press “n” to display the next iteration

### Backend:

#### Pathfinding with A\* Algorithm

The A\* algorithm will check for a viable path from the player to the goal, taking into account the upcoming cell state. If no path is found, the target is moved to a viable cell.

## Implementation Plan:

| Description | Time |
| :---- | :---- |
| Web app (just starting/accessing it) Server Static files to serve to browser | Nov 4: get basic web server up on vps and make sure it can call functions from ocaml on the browser |
| Default Conway Implementation (only simulation, no player input) | Nov 4: Just default conway simulation, no player input yet  |
| Add player input | Nov 11 |
| Implement custom rules | Nov 13 |
| MVP done (w/out algorithm) | Nov 18 |
| A\* pathfinding algorithm done | Nov 21 |
| Testing | Nov 25 onwards |
| Final version done | Dec 10 |

## 

[image1]: <data:image/png;base64,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>

[image2]: <data:image/png;base64,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>

[image3]: <data:image/png;base64,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>

[image4]: <data:image/png;base64,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2mYURMdymyeCL+/Ur9s06fwEnjgkiM/f24yov3kfhdskod4rN/cJIg7laByzzt05mnHO1L+P73P1a/UxT3Qdy9LPgoj5nIy2Gc/lkCAat1e08QvuWGkRZC8qviaq//VrvtZ/rGhSHvNlA7w9DyoIwGMHQQAWIAjAAgQBWIAgAAsQBGABggAsQBCABQgCsABBABYgCMACBAFYgCAACxAEYAGCACxAEIAFCAKwAEEAFiAIwAIEAViAIAALyoL8/vvvRxUAW2gR5Oeff76dgL/99tudSbkl9L/x/Oyzz64//vjjO591BMAWWgTRhP7yyy9vXv/yyy+3k/Hbb7+9/vzzz2/bfvzxx5uf33///e024/aKH3744fb1e++9dyOc4quvvrrZT6+1j7ZT32rTZ+5bIWF1PnPfCoAttAiiiahvfv30pNR7T0qtBpq0mvBeITTRx5XH/fi1BFBYEPf5wQcf3OznFUZy6Kdl0efux5+NAbCFNkEswrha6L1CYmjyahJrVdHE97f8OHnHyzNtq/20nULvP/zww78U5P333789rrZFEKjQKoh+alJ7srtdK4Zeq+2jjz66adNE9usxvAJoW09wbye5/koQtfsyjRUEqrQK4kk8TnZN2K+//vq2zauG2ucVxPtrP4nmfj/99NObti+++OIvBdE+2sbHnvsH2EKLIMcUAFsoCwLwTwZBABYgCMACBAFYgCAACxAEYAGCACxAEIAFCAKwAEEAFiAIwAIEAViAIAALEARgAYIALEAQgAUIArAAQQAWIAjAAgQBWIAgAAsQBGABggAsQBCABQgCsABBABYgCMACBAFYgCAACxAEYAGCACxAEIAFCAKw4D89RgLH06AwxgAAAABJRU5ErkJggg==>