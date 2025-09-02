# ⚖️ Resolvr - Decentralized Arbitration Smart Contract

A decentralized dispute resolution platform built on Stacks blockchain using Clarity smart contracts. Resolvr enables parties to resolve disputes through community arbitration without traditional legal systems.

## 🌟 Features

- **Dispute Creation**: Create disputes with stake amounts and descriptions
- **Arbitrator Network**: Decentralized network of staked arbitrators
- **Voting System**: Democratic resolution through arbitrator votes
- **Reputation System**: Track arbitrator performance and reliability
- **Automated Resolution**: Smart contract handles fund distribution
- **Stake-based Security**: Arbitrators must stake STX to participate

## 🚀 How It Works

### For Dispute Parties

1. **Create Dispute** 📝
   - Plaintiff creates dispute with stake amount
   - Defendant responds with counter-stake
   - Funds are locked in contract

2. **Resolution** ✅
   - Arbitrators vote on the dispute
   - Majority vote determines winner
   - Winner receives all staked funds

### For Arbitrators

1. **Registration** 🔐
   - Stake minimum 5 STX to become arbitrator
   - Build reputation through fair voting
   - Earn fees for participation

2. **Voting** 🗳️
   - Vote on active disputes
   - Voting period: 144 blocks (~24 hours)
   - Reputation increases with participation

## 📋 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `register-arbitrator` | Register as arbitrator with stake | `stake-amount: uint` |
| `create-dispute` | Create new dispute | `defendant: principal, amount: uint, description: string` |
| `respond-to-dispute` | Defendant responds with counter-stake | `dispute-id: uint, counter-stake: uint` |
| `vote-on-dispute` | Arbitrator votes on dispute | `dispute-id: uint, vote-for: string` |
| `resolve-dispute` | Resolve dispute and distribute funds | `dispute-id: uint` |
| `withdraw-arbitrator-stake` | Withdraw stake and deactivate | - |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-dispute` | Get dispute details | `dispute object` |
| `get-arbitrator` | Get arbitrator info | `arbitrator object` |
| `get-dispute-vote` | Get specific vote | `vote object` |
| `can-resolve-dispute` | Check if dispute can be resolved | `boolean` |

## 💻 Usage Examples

### Creating a Dispute

```clarity
(contract-call? .Resolvr create-dispute 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u1000000 "Payment dispute for services")
```

### Registering as Arbitrator

```clarity
(contract-call? .Resolvr register-arbitrator u5000000)
```

### Voting on Dispute

```clarity
(contract-call? .Resolvr vote-on-dispute u1 "plaintiff")
```

## 🔧 Configuration

- **Minimum Arbitrator Stake**: 5 STX
- **Voting Period**: 144 blocks (~24 hours)
- **Minimum Votes for Resolution**: 3 arbitrators

## 🛡️ Security Features

- Stake-based arbitrator system prevents spam
- Time-locked voting periods ensure fair process
- Reputation system encourages honest behavior
- Contract holds funds securely until resolution

## 🎯 Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Dispute not found |
| u102 | Invalid status |
| u103 | Insufficient funds |
| u104 | Already voted |
| u105 | Not arbitrator |
| u106 | Voting ended |
| u107 | Dispute resolved |
| u108 | Invalid party |

## 🚀 Getting Started

1. Deploy the contract to Stacks blockchain
2. Register as arbitrator or create disputes
3. Participate in the decentralized justice system

## 📈 Future Enhancements

- Multi-token support
- Appeal mechanisms
- Advanced reputation algorithms
- Integration with legal frameworks


