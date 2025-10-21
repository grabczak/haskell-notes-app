# Haskell Notes App

A simple RESTful backend API for managing notes, written in **Haskell**.  
Built as a minimal example of how to design and implement a backend service using **Servant**.

## 🚀 Features

- CRUD API for notes (create, read, update, delete)
- JSON request/response via [Aeson](https://hackage.haskell.org/package/aeson)
- Type-safe routing with [Servant](https://hackage.haskell.org/package/servant)
- Persistent storage (SQLite)

## 📦 Setup

### Prerequisites

Use [GHCUp](https://www.haskell.org/ghcup/) to install GHC, Stack and Cabal.

### Build and Run

Clone the repository and run:

```bash
stack build
stack run
```
