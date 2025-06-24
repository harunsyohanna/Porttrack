# 🚢 Porttrack - Shipping Container Registry

A Clarity smart contract for tracking shipping containers across global ports on the Stacks blockchain.

## 📋 Overview

Porttrack enables shipping companies and port operators to register, track, and manage shipping containers throughout their journey across different ports. The contract maintains a complete history of container movements and status updates.

## ✨ Features

- 📦 **Container Registration**: Register new containers with cargo details
- 🔄 **Status Tracking**: Update container status and location in real-time  
- 📍 **Port Management**: Authorize and manage port operators
- 📊 **History Tracking**: Complete audit trail of container movements
- 🔐 **Access Control**: Role-based permissions for operators and ports
- 📈 **Port Statistics**: Track container counts per port

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
clarinet new porttrack-project
cd porttrack-project
```

Copy the contract code to `contracts/Porttrack.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage

### Initialize the System

First, authorize ports and operators:

```clarity
(contract-call? .Porttrack authorize-port "PORT-OF-SINGAPORE")
(contract-call? .Porttrack authorize-operator 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### Register a Container

```clarity
(contract-call? .Porttrack register-container 
  "CONT123456789" 
  "PORT-OF-SINGAPORE" 
  "PORT-OF-ROTTERDAM" 
  "Electronics and machinery" 
  u25000)
```

### Update Container Status

```clarity
(contract-call? .Porttrack update-container-status 
  u1 
  "PORT-OF-DUBAI" 
  "in-transit")
```

### Query Container Information

```clarity
(contract-call? .Porttrack get-container u1)
(contract-call? .Porttrack get-container-history u1 u0)
```

## 📊 Container Status Types

- `registered` - Container registered in system
- `loading` - Container being loaded
- `in-transit` - Container in transit between ports
- `arrived` - Container arrived at port
- `unloading` - Container being unloaded
- `customs` - Container in customs processing
- `released` - Container released from customs
- `delivered` - Container delivered to final destination

## 🔧 Functions

### Public Functions

- `register-container` - Register a new container
- `update-container-status` - Update container location and status
- `authorize-port` - Authorize a port (owner only)
- `revoke-port` - Revoke port authorization (owner only)
- `authorize-operator` - Authorize an operator (owner only)
- `revoke-operator` - Revoke operator authorization (owner only)

### Read-Only Functions

- `get-container` - Get container details
- `get-container-history` - Get container movement history
- `get-port-container-count` - Get container count at port
- `is-port-authorized` - Check if port is authorized
- `is-authorized-operator` - Check if operator is authorized
- `get-next-container-id` - Get next available container ID

## 🛡️ Security

- Contract owner has administrative privileges
- Port operators must be authorized to register/update containers
- Ports must be authorized before containers can be registered there
- Complete audit trail maintained for all operations

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

