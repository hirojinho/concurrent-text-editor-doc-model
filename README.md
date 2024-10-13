# Actor Model with Erlang/Elixir

## Create the Erlang/Elixir Backend

    Each user will be represented as an actor (process) in Erlang/Elixir. You can either directly handle WebSocket connections in Elixir (using Phoenix Channels) or forward user actions from Go to Erlang/Elixir through APIs.
    Each actor will be responsible for managing a user’s session and the specific edits they make to the document.
    Use the GenServer module (in Elixir) to model actors, with each actor handling the state of individual users.

## Synchronize with Go Server

    If using Elixir, set up communication between Go and Erlang/Elixir backend, possibly through an API or message queue (like RabbitMQ).
    Ensure the actor system models different versions of the document and can handle conflicts or merge changes based on user inputs.

    System Architecture Overview

Your real-time collaboration system can be broken down into the following components:

    User Actors: Represent each user interacting with the document.
    Document Actor: Represents the document as a whole. It tracks the current version of the document and coordinates updates.
    Change Actors: Represent individual changes to the document. When a user makes an edit, a new change actor is spawned to handle that edit. The change actor applies the modification and updates the document state.
    Supervisor: Supervises the document and change actors to ensure they are restarted if they fail.

Architectural Components in Detail
1. User Actor

    Purpose: Represents a connected user.
    Responsibility: Receives updates from the client (via WebSocket or HTTP) and sends changes to the document actor. It also listens for document updates (e.g., changes made by other users) and pushes these back to the client.

2. Document Actor

    Purpose: Central authority for managing document state.
    Responsibility:
        Stores the current document state (which can be a simple text structure like a string, or a more complex data structure like a tree).
        Coordinates changes: when a change request is received, the document actor spawns a new change actor to handle it.
        Maintains version history: tracks versions to allow real-time synchronization between users.
        Synchronizes all users: after applying a change, the document actor broadcasts the updated version to all connected users.

3. Change Actor

    Purpose: Isolate each document change and apply it independently.
    Responsibility: Each change actor:
        Takes a specific change (e.g., inserting or deleting text) and applies it to the document.
        Updates the document state and passes the result back to the document actor.
        When the document actor approves, it updates the version and broadcasts the updated document to all user actors.

4. Supervisor

    Purpose: Ensure system reliability.
    Responsibility: Monitors all document and change actors. If any process crashes, the supervisor automatically restarts it according to a specified strategy (e.g., restart one process, or restart all processes).

3. Message Passing Between Actors

Erlang’s actors communicate by sending messages. Here’s how this works in your system:

    User to Document Actor:
        When a user makes a change, the user actor sends a message to the document actor: {apply_change, UserId, Change}.
    Document Actor to Change Actor:
        The document actor spawns a change actor to process the change and sends a message: {apply_change, DocState, Change}.
    Change Actor Back to Document Actor:
        After applying the change, the change actor sends the updated document back to the document actor: {change_applied, NewDocState}.
    Document Actor to All User Actors:
        The document actor updates its state and broadcasts the new document to all connected user actors: {broadcast_change, NewDocState}


        Example Erlang Architecture for Real-Time Collaboration

Here’s a breakdown of the architecture and message flow for a typical user edit:

    User Makes a Change:
        The user actor receives the change from the front end and sends a message to the document actor: {apply_change, UserId, Change}.

    Document Actor Spawns Change Actor:
        The document actor spawns a new process (change actor) to handle the change.
        The document actor sends the current document state and the change to the change actor.

    Change Actor Applies the Change:
        The change actor applies the modification and returns the updated document state to the document actor: {change_applied, NewDocState}.

    Document Actor Updates the Document:
        The document actor updates its version of the document and increments the version number.
        It broadcasts the new document state to all connected user actors: {broadcast_change, NewDocState}.

    User Actors Notify Clients:
        The user actors receive the updated document state and push it to their respective clients via WebSocket.