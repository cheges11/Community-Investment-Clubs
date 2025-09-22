;; Community Investment Club Smart Contract
;; Enables local investment groups with shared decision-making

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CLUB-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-MEMBER (err u102))
(define-constant ERR-NOT-MEMBER (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-VOTING-ENDED (err u107))
(define-constant ERR-VOTING-ACTIVE (err u108))
(define-constant ERR-INVALID-AMOUNT (err u109))
(define-constant ERR-CLUB-FULL (err u110))

;; Constants
(define-constant MAX-MEMBERS u50)
(define-constant MIN-VOTING-PERIOD u144) ;; ~1 day in blocks
(define-constant MAX-VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant PROPOSAL-DEPOSIT u1000000) ;; 1 STX in microSTX

;; Data variables
(define-data-var club-counter uint u0)
(define-data-var proposal-counter uint u0)

;; Club structure
(define-map clubs
  uint
  {
    name: (string-ascii 50),
    creator: principal,
    total-funds: uint,
    member-count: uint,
    min-contribution: uint,
    voting-threshold: uint, ;; percentage (0-100)
    created-at: uint
  }
)

;; Club membership
(define-map club-members
  { club-id: uint, member: principal }
  {
    contribution: uint,
    joined-at: uint,
    is-active: bool
  }
)

;; Investment proposals
(define-map proposals
  uint
  {
    club-id: uint,
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    investment-amount: uint,
    target-address: (optional principal),
    votes-for: uint,
    votes-against: uint,
    voting-end: uint,
    executed: bool,
    created-at: uint
  }
)

;; Proposal votes tracking
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

;; Club member list for iteration
(define-map club-member-list
  { club-id: uint, index: uint }
  principal
)

;; Helper functions
(define-private (is-club-member (club-id uint) (member principal))
  (is-some (map-get? club-members { club-id: club-id, member: member }))
)

(define-private (get-member-data (club-id uint) (member principal))
  (map-get? club-members { club-id: club-id, member: member })
)

(define-private (calculate-voting-power (club-id uint) (member principal))
  (match (get-member-data club-id member)
    member-data (get contribution member-data)
    u0
  )
)

;; Create a new investment club
(define-public (create-club 
  (name (string-ascii 50))
  (min-contribution uint)
  (voting-threshold uint))
  (let
    (
      (club-id (+ (var-get club-counter) u1))
      (creator tx-sender)
    )
    (asserts! (and (>= voting-threshold u1) (<= voting-threshold u100)) ERR-INVALID-AMOUNT)
    (asserts! (> min-contribution u0) ERR-INVALID-AMOUNT)
    
    ;; Create club
    (map-set clubs club-id
      {
        name: name,
        creator: creator,
        total-funds: u0,
        member-count: u0,
        min-contribution: min-contribution,
        voting-threshold: voting-threshold,
        created-at: block-height
      }
    )
    
    (var-set club-counter club-id)
    (ok club-id)
  )
)

;; Join a club with initial contribution
(define-public (join-club (club-id uint) (contribution uint))
  (let
    (
      (club-data (unwrap! (map-get? clubs club-id) ERR-CLUB-NOT-FOUND))
      (member tx-sender)
      (current-member-count (get member-count club-data))
    )
    (asserts! (not (is-club-member club-id member)) ERR-ALREADY-MEMBER)
    (asserts! (>= contribution (get min-contribution club-data)) ERR-INVALID-AMOUNT)
    (asserts! (< current-member-count MAX-MEMBERS) ERR-CLUB-FULL)
    
    ;; Verify sufficient balance before transfer
    (asserts! (>= (stx-get-balance tx-sender) contribution) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer contribution to contract
    (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
    
    ;; Add member
    (map-set club-members 
      { club-id: club-id, member: member }
      {
        contribution: contribution,
        joined-at: block-height,
        is-active: true
      }
    )
    
    ;; Add to member list
    (map-set club-member-list
      { club-id: club-id, index: current-member-count }
      member
    )
    
    ;; Update club data
    (map-set clubs club-id
      (merge club-data
        {
          total-funds: (+ (get total-funds club-data) contribution),
          member-count: (+ current-member-count u1)
        }
      )
    )
    
    (ok true)
  )
)

;; Add additional contribution to club
(define-public (add-contribution (club-id uint) (amount uint))
  (let
    (
      (club-data (unwrap! (map-get? clubs club-id) ERR-CLUB-NOT-FOUND))
      (member-data (unwrap! (get-member-data club-id tx-sender) ERR-NOT-MEMBER))
      (member tx-sender)
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (get is-active member-data) ERR-NOT-MEMBER)
    
    ;; Verify sufficient balance before transfer
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer additional contribution
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update member contribution
    (map-set club-members
      { club-id: club-id, member: member }
      (merge member-data
        { contribution: (+ (get contribution member-data) amount) }
      )
    )
    
    ;; Update club total funds
    (map-set clubs club-id
      (merge club-data
        { total-funds: (+ (get total-funds club-data) amount) }
      )
    )
    
    (ok true)
  )
)

;; Create investment proposal
(define-public (create-proposal
  (club-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (investment-amount uint)
  (target-address (optional principal))
  (voting-period uint))
  (let
    (
      (club-data (unwrap! (map-get? clubs club-id) ERR-CLUB-NOT-FOUND))
      (proposal-id (+ (var-get proposal-counter) u1))
      (proposer tx-sender)
    )
    (asserts! (is-club-member club-id proposer) ERR-NOT-MEMBER)
    (asserts! (> investment-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= investment-amount (get total-funds club-data)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (and (>= voting-period MIN-VOTING-PERIOD) 
                   (<= voting-period MAX-VOTING-PERIOD)) ERR-INVALID-AMOUNT)
    
    ;; Verify sufficient balance for proposal deposit
    (asserts! (>= (stx-get-balance tx-sender) PROPOSAL-DEPOSIT) ERR-INSUFFICIENT-FUNDS)
    
    ;; Charge proposal deposit
    (try! (stx-transfer? PROPOSAL-DEPOSIT tx-sender (as-contract tx-sender)))
    
    ;; Create proposal
    (map-set proposals proposal-id
      {
        club-id: club-id,
        proposer: proposer,
        title: title,
        description: description,
        investment-amount: investment-amount,
        target-address: target-address,
        votes-for: u0,
        votes-against: u0,
        voting-end: (+ block-height voting-period),
        executed: false,
        created-at: block-height
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (club-id (get club-id proposal-data))
      (voter tx-sender)
      (voting-power (calculate-voting-power club-id voter))
    )
    (asserts! (is-club-member club-id voter) ERR-NOT-MEMBER)
    (asserts! (< block-height (get voting-end proposal-data)) ERR-VOTING-ENDED)
    (asserts! (is-none (map-get? proposal-votes 
                       { proposal-id: proposal-id, voter: voter })) ERR-ALREADY-VOTED)
    
    ;; Record vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: voter }
      { vote: vote-for, voted-at: block-height }
    )
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id
      (merge proposal-data
        {
          votes-for: (if vote-for 
                        (+ (get votes-for proposal-data) voting-power)
                        (get votes-for proposal-data)),
          votes-against: (if vote-for
                           (get votes-against proposal-data)
                           (+ (get votes-against proposal-data) voting-power))
        }
      )
    )
    
    (ok true)
  )
)

;; Execute approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (club-data (unwrap! (map-get? clubs (get club-id proposal-data)) ERR-CLUB-NOT-FOUND))
      (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
      (approval-percentage (if (> total-votes u0)
                             (/ (* (get votes-for proposal-data) u100) total-votes)
                             u0))
    )
    (asserts! (>= block-height (get voting-end proposal-data)) ERR-VOTING-ACTIVE)
    (asserts! (not (get executed proposal-data)) ERR-VOTING-ACTIVE)
    (asserts! (>= approval-percentage (get voting-threshold club-data)) ERR-NOT-AUTHORIZED)
    
    ;; Verify contract has sufficient balance for investment
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) 
                  (get investment-amount proposal-data)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Execute investment
    (match (get target-address proposal-data)
      target-addr (try! (as-contract (stx-transfer? (get investment-amount proposal-data)
                                                   tx-sender target-addr)))
      ;; If no target address, keep funds in contract for manual distribution
      true
    )
    
    ;; Mark as executed
    (map-set proposals proposal-id
      (merge proposal-data { executed: true })
    )
    
    ;; Update club funds
    (map-set clubs (get club-id proposal-data)
      (merge club-data
        { total-funds: (- (get total-funds club-data) 
                         (get investment-amount proposal-data)) }
      )
    )
    
    ;; Return deposit to proposer
    (try! (as-contract (stx-transfer? PROPOSAL-DEPOSIT tx-sender (get proposer proposal-data))))
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-club-info (club-id uint))
  (map-get? clubs club-id)
)

(define-read-only (get-member-info (club-id uint) (member principal))
  (map-get? club-members { club-id: club-id, member: member })
)

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-club-total-funds (club-id uint))
  (match (map-get? clubs club-id)
    club-data (some (get total-funds club-data))
    none
  )
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))
)