#![cfg_attr(not(feature = "std"), no_std, no_main)]

pub use self::gtx::{
    Gtx,
    GtxRef,
};

#[ink::contract]
pub mod gtx {
    use ink::storage::Mapping;
    use ink_prelude::borrow::ToOwned;
    use ink_prelude::string::String;
    use scale::{
        Decode,
        Encode,
    };

    /// A simple ERC-20 contract.
    #[ink(storage)]
    pub struct Gtx {
        /// Total token supply.
        total_supply: Balance,
        /// Mapping from owner to number of owned token.
        balances: Mapping<AccountId, Balance>,
        /// Mapping of the token amount which an account is allowed to withdraw
        /// from another account.
        allowances: Mapping<(AccountId, AccountId), Balance>,
        /// Name of the token
        token_name: String,
        /// Symbol the token
        token_symbol: String,
        /// Decimal places of the token
        decimals: u8,
        /// Owner of the token
        token_owner: AccountId,
    }

    #[derive(Encode, Decode, Debug)]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    pub struct Metadata {
        total_supply: Balance,
        decimals: u8,
        token_symbol: String,
        token_name: String,
    }

    /// Event emitted when a token transfer occurs.
    #[ink(event)]
    pub struct Transfer {
        #[ink(topic)]
        from: Option<AccountId>,
        #[ink(topic)]
        to: Option<AccountId>,
        value: Balance,
    }

    /// Event emitted when an approval occurs that `spender` is allowed to withdraw
    /// up to the amount of `value` tokens from `owner`.
    #[ink(event)]
    pub struct Approval {
        #[ink(topic)]
        owner: AccountId,
        #[ink(topic)]
        spender: AccountId,
        value: Balance,
    }

    /// The ERC-20 error types.
    #[derive(Debug, PartialEq, Eq, Encode, Decode)]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    pub enum Error {
        /// Returned if not enough balance to fulfill a request is available.
        InsufficientBalance,
        /// Returned if not enough allowance to fulfill a request is available.
        InsufficientAllowance,
        /// Returned if the caller is not an owner of the token.
        Unauthorized
    }

    /// The ERC-20 result type.
    pub type Result<T> = core::result::Result<T, Error>;

    impl Metadata {
        fn get_info(
            total_supply: Balance,
            decimals: u8,
            token_symbol: &str,
            token_name: &str
        ) -> Metadata {
            Metadata {
                total_supply,
                decimals,
                token_symbol: token_symbol.to_owned(),
                token_name: token_name.to_owned(),
            }
        }
    }

    impl Gtx {
        /// Creates a new ERC-20 contract with the specified parameters.
        #[ink(constructor)]
        pub fn new(
            token_name: String,
            token_symbol: String,
            decimals: u8,
            token_owner: AccountId,
            total_supply: Balance,
            mint_to: AccountId,
        ) -> Self {
            let total_supply_scaled = total_supply / 10u128.pow(decimals as u32);
            let mut balances = Mapping::default();
            let caller: ink::primitives::AccountId = Self::env().caller();
            balances.insert(mint_to, &total_supply_scaled);
            Self::env().emit_event(Transfer {
                from: None,
                to: Some(caller),
                value: total_supply,
            });
            Self {
                total_supply: total_supply_scaled,
                balances,
                allowances: Default::default(),
                token_name,
                token_symbol,
                decimals,
                token_owner,
            }
        }

        /// Allows to change the contract `token_owner`.
        #[ink(message)]
        pub fn change_owner(&mut self, new_owner: AccountId) -> Result<()> {
            let caller = self.env().caller();
            if caller != self.token_owner {
                return Err(Error::Unauthorized);
            }
            self.token_owner = new_owner;
            Ok(())
        }

        /// Returns the metadata of the token.
        #[ink(message)]
        pub fn metadata(&self) -> Metadata {
            Metadata::get_info(
                self.total_supply, 
                self.decimals, 
                &self.token_symbol, 
                &self.token_name
            )
        }

        /// Returns the `decimals` value of the token.
        #[ink(message)]
        pub fn get_decimals(&self) -> u8 {
            self.decimals
        }
        
        /// Returns the `token_symbol` value of the token.
        #[ink(message)]
        pub fn get_symbol(&self) -> String {
            self.token_symbol.clone()
        }

        /// Returns the `token_name` value of the token.
        #[ink(message)]
        pub fn get_name(&self) -> String {
            self.token_name.clone()
        }

        /// Returns the total token supply.
        #[ink(message)]
        pub fn total_supply(&self) -> Balance {
            self.total_supply
        }

        /// Returns the account balance for the specified `owner`.
        ///
        /// Returns `0` if the account is non-existent.
        #[ink(message)]
        pub fn balance_of(&self, owner: AccountId) -> Balance {
            self.balance_of_impl(&owner)
        }

        /// Returns the account balance for the specified `owner`.
        ///
        /// Returns `0` if the account is non-existent.
        ///
        /// # Note
        ///
        /// Prefer to call this method over `balance_of` since this
        /// works using references which are more efficient in Wasm.
        #[inline]
        fn balance_of_impl(&self, owner: &AccountId) -> Balance {
            self.balances.get(owner).unwrap_or_default()
        }

        /// Returns the amount which `spender` is still allowed to withdraw from `owner`.
        ///
        /// Returns `0` if no allowance has been set.
        #[ink(message)]
        pub fn allowance(&self, owner: AccountId, spender: AccountId) -> Balance {
            self.allowance_impl(&owner, &spender)
        }

        /// Returns the amount which `spender` is still allowed to withdraw from `owner`.
        ///
        /// Returns `0` if no allowance has been set.
        ///
        /// # Note
        ///
        /// Prefer to call this method over `allowance` since this
        /// works using references which are more efficient in Wasm.
        #[inline]
        fn allowance_impl(&self, owner: &AccountId, spender: &AccountId) -> Balance {
            self.allowances.get((owner, spender)).unwrap_or_default()
        }

        /// Transfers `value` amount of tokens from the caller's account to account `to`.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the caller's account balance.
        #[ink(message)]
        pub fn transfer(
            &mut self, 
            caller: AccountId, 
            to: AccountId, 
            value: Balance
        ) -> Result<()> {
            let from = caller;
            self.transfer_from_to(&from, &to, value)
        }

        /// Add `to` to mint new tokens with the `value` specified
        ///
        /// If this function is called again it overwrites the current allowance with
        /// `value`.
        ///
        /// A `Mint` event is emitted.
        #[ink(message)]
        pub fn mint(
            &mut self, 
            caller: AccountId, 
            to: AccountId, 
            value: Balance
        ) -> Result<()> {
            // Ensure only the contract owner or a specific role can mint tokens
            if caller != self.token_owner {
                return Err(Error::Unauthorized);
            }
            let scaled_value = value / 10u128.pow(self.decimals as u32);

            // Mint new tokens and update the total supply
            self.total_supply += scaled_value;
            
            // Update the balance of the recipient
            let recipient_balance = self.balance_of(to);
            self.balances.insert(to, &(recipient_balance + scaled_value));

            // Emit a Transfer event to indicate the minting
            self.env().emit_event(Transfer {
                from: None,
                to: Some(to),
                value: scaled_value,
            });
            Ok(())
        }

        /// Remove `from` tokens base on the `value` given parameter
        /// ///
        /// Returns `Unauthorized` error if the caller is not an owner
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the account balance of `from` to burn.
        #[ink(message)]
        pub fn burn(
            &mut self, 
            caller: AccountId, 
            from: AccountId, 
            value: Balance
        ) -> Result<()> {
            // Ensure only the owner or a specific role can burn tokens
            if caller != self.token_owner {
                return Err(Error::Unauthorized);
            }
            let scaled_value = value / 10u128.pow(self.decimals as u32);
            // Ensure the account has enough balance to burn
            let sender_balance = self.balance_of(from);
            if sender_balance < scaled_value {
                return Err(Error::InsufficientBalance);
            }

            // Burn tokens and update the total supply
            self.total_supply -= scaled_value;

            // Update the balance of the sender
            self.balances.insert(from, &(sender_balance - scaled_value));

            // Emit a Transfer event to indicate the burning
            self.env().emit_event(Transfer {
                from: Some(from),
                to: None,
                value: scaled_value,
            });
            Ok(())
        }


        /// Allows `spender` to withdraw from the caller's account multiple times, up to
        /// the `value` amount.
        ///
        /// If this function is called again it overwrites the current allowance with
        /// `value`.
        ///
        /// An `Approval` event is emitted.
        #[ink(message)]
        pub fn approve(&mut self, spender: AccountId, value: Balance) -> Result<()> {
            let owner = self.env().caller();
            let scaled_value = value / 10u128.pow(self.decimals as u32);
            self.allowances.insert((&owner, &spender), &value);
            self.env().emit_event(Approval {
                owner,
                spender,
                value: scaled_value,
            });
            Ok(())
        }

        /// Transfers `value` tokens on the behalf of `from` to the account `to`.
        ///
        /// This can be used to allow a contract to transfer tokens on ones behalf and/or
        /// to charge fees in sub-currencies, for example.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientAllowance` error if there are not enough tokens allowed
        /// for the caller to withdraw from `from`.
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the account balance of `from`.
        #[ink(message)]
        pub fn transfer_from(
            &mut self,
            from: AccountId,
            to: AccountId,
            value: Balance,
        ) -> Result<()> {
            let caller = self.env().caller();
            let allowance = self.allowance_impl(&from, &caller);
            if allowance < value {
                return Err(Error::InsufficientAllowance)
            }
            self.transfer_from_to(&from, &to, value)?;
            self.allowances
                .insert((&from, &caller), &(allowance - value));
            Ok(())
        }

        /// Transfers `value` amount of tokens from the caller's account to account `to`.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the caller's account balance.
        fn transfer_from_to(
            &mut self,
            from: &AccountId,
            to: &AccountId,
            value: Balance,
        ) -> Result<()> {
            let scaled_value = value / 10u128.pow(self.decimals as u32);
            let from_balance = self.balance_of_impl(from);
            if from_balance < scaled_value {
                return Err(Error::InsufficientBalance)  
            }
            self.balances.insert(from, &(from_balance - scaled_value));
            let to_balance = self.balance_of_impl(to);
            self.balances.insert(to, &(to_balance + scaled_value));
            self.env().emit_event(Transfer {
                from: Some(*from),
                to: Some(*to),
                value: scaled_value,
            });
            Ok(())
        }

        /// Updates the contract code using `set_code_hash()`
        /// Returns `Unauthorized` error if the caller is not an owner
        #[ink(message)]
        pub fn set_code(&mut self, code_hash: Hash) -> Result<()> {
            let caller = self.env().caller();
            if caller != self.token_owner {
                return Err(Error::Unauthorized);
            }
            self.env().set_code_hash(&code_hash).unwrap_or_else(|err| {
                panic!("Failed to `set_code_hash` to {code_hash:?} due to {err:?}")
            });
            ink::env::debug_println!("Switched code hash to {:?}.", code_hash);
            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        use ink::primitives::{
            Clear,
            Hash,
        };

        type Event = <Gtx as ::ink::reflect::ContractEventBase>::Type;

        fn assert_transfer_event(
            event: &ink::env::test::EmittedEvent,
            expected_from: Option<AccountId>,
            expected_to: Option<AccountId>,
            expected_value: Balance,
        ) {
            let decoded_event = <Event as scale::Decode>::decode(&mut &event.data[..])
                .expect("encountered invalid contract event data buffer");
            if let Event::Transfer(Transfer { from, to, value }) = decoded_event {
                assert_eq!(from, expected_from, "encountered invalid Transfer.from");
                assert_eq!(to, expected_to, "encountered invalid Transfer.to");
                assert_eq!(value, expected_value, "encountered invalid Trasfer.value");
            } else {
                panic!("encountered unexpected event kind: expected a Transfer event")
            }
            let expected_topics = vec![
                encoded_into_hash(&PrefixedValue {
                    value: b"Gtx::Transfer",
                    prefix: b"",
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Gtx::Transfer::from",
                    value: &expected_from,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Gtx::Transfer::to",
                    value: &expected_to,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Gtx::Transfer::value",
                    value: &expected_value,
                }),
            ];

            let topics = event.topics.clone();
            for (n, (actual_topic, expected_topic)) in
                topics.iter().zip(expected_topics).enumerate()
            {
                let mut topic_hash = Hash::CLEAR_HASH;
                let len = actual_topic.len();
                topic_hash.as_mut()[0..len].copy_from_slice(&actual_topic[0..len]);

                assert_eq!(
                    topic_hash, expected_topic,
                    "encountered invalid topic at {n}"
                );
            }
        }

        /// Mint a token
        #[ink::test]
        fn test_mint() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let _erc20 = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );

            // Act
            let amount_to_mint: Balance = 500;
            let result = contract.mint(accounts.alice, amount_to_mint);

            // Assert
            assert_eq!(result, Ok(()));
            assert_eq!(contract.total_supply, initial_supply + amount_to_mint);
            assert_eq!(contract.balance_of(accounts.alice), amount_to_mint);
        }

        /// Burn a token
        #[ink::test]
        fn test_burn() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let _erc20 = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );

            // Act
            let amount_to_burn: Balance = 500;
            let result = contract.burn(accounts.alice, amount_to_burn);

            // Assert
            assert_eq!(result, Ok(()));
            assert_eq!(contract.total_supply, initial_supply - amount_to_burn);
            assert_eq!(contract.balance_of(accounts.alice), initial_supply - amount_to_burn);
        }

        /// The default constructor does its job.
        #[ink::test]
        fn new_works() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let _erc20 = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );

            // Transfer event triggered during initial construction.
            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(1, emitted_events.len());

            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(accounts.alice),
                100,
            );
        }

        /// The total supply was applied.
        #[ink::test]
        fn total_supply_works() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            // Transfer event triggered during initial construction.
            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(accounts.alice),
                100,
            );
            // Get the token total supply.
            assert_eq!(gtx.total_supply(), 100);
        }

        /// Get the actual balance of an account.
        #[ink::test]
        fn balance_of_works() {
            // Constructor works
            let accounts = ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            
            // Transfer event triggered during initial construction
            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            let accounts =
                ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            // Alice owns all the tokens on contract instantiation
            assert_eq!(gtx.balance_of(accounts.alice), 100);
            // Bob does not owns tokens
            assert_eq!(gtx.balance_of(accounts.bob), 0);
        }


        #[ink::test]
        fn transfer_works() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let mut gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            // Transfer event triggered during initial construction.
            assert_eq!(gtx.balance_of(accounts.bob), 0);
            // Alice transfers 10 tokens to Bob.
            assert_eq!(gtx.transfer(accounts.bob, 10), Ok(()));
            // Bob owns 10 tokens.
            assert_eq!(gtx.balance_of(accounts.bob), 10);

            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 2);
            // Check first transfer event related to ERC-20 instantiation.
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // Check the second transfer event relating to the actual trasfer.
            assert_transfer_event(
                &emitted_events[1],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x02; 32])),
                10,
            );
        }

        #[ink::test]
        fn invalid_transfer_should_fail() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let mut gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            assert_eq!(gtx.balance_of(accounts.bob), 0);

            // Set the contract as callee and Bob as caller.
            let contract = ink::env::account_id::<ink::env::DefaultEnvironment>();
            ink::env::test::set_callee::<ink::env::DefaultEnvironment>(contract);
            ink::env::test::set_caller::<ink::env::DefaultEnvironment>(accounts.bob);

            // Bob fails to transfers 10 tokens to Eve.
            assert_eq!(
                gtx.transfer(accounts.eve, 10),
                Err(Error::InsufficientBalance)
            );
            // Alice owns all the tokens.
            assert_eq!(gtx.balance_of(accounts.alice), 100);
            assert_eq!(gtx.balance_of(accounts.bob), 0);
            assert_eq!(gtx.balance_of(accounts.eve), 0);

            // Transfer event triggered during initial construction.
            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 1);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
        }

        #[ink::test]
        fn transfer_from_works() {
            // Constructor works.
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let mut gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            // Bob fails to transfer tokens owned by Alice.
            assert_eq!(
                gtx.transfer_from(accounts.alice, accounts.eve, 10),
                Err(Error::InsufficientAllowance)
            );
            // Alice approves Bob for token transfers on her behalf.
            assert_eq!(gtx.approve(accounts.bob, 10), Ok(()));

            // The approve event takes place.
            assert_eq!(ink::env::test::recorded_events().count(), 2);

            // Set the contract as callee and Bob as caller.
            let contract = ink::env::account_id::<ink::env::DefaultEnvironment>();
            ink::env::test::set_callee::<ink::env::DefaultEnvironment>(contract);
            ink::env::test::set_caller::<ink::env::DefaultEnvironment>(accounts.bob);

            // Bob transfers tokens from Alice to Eve.
            assert_eq!(
                gtx.transfer_from(accounts.alice, accounts.eve, 10),
                Ok(())
            );
            // Eve owns tokens.
            assert_eq!(gtx.balance_of(accounts.eve), 10);

            // Check all transfer events that happened during the previous calls:
            let emitted_events = ink::env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 3);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // The second event `emitted_events[1]` is an Approve event that we skip
            // checking.
            assert_transfer_event(
                &emitted_events[2],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x05; 32])),
                10,
            );
        }

        #[ink::test]
        fn allowance_must_not_change_on_failed_transfer() {
            let accounts =
            ink::env::test::default_accounts::<ink::env::DefaultEnvironment>();
            let token_name = String::from("MyToken");
            let token_symbol = String::from("MTK");
            let decimals: u8 = 18;
            let total_supply: Balance = 100;
            let token_owner = accounts.alice;
            let mint_to = accounts.alice;
            let mut gtx = Gtx::new(
                token_name.clone(),
                token_symbol.clone(),
                decimals,
                token_owner,
                total_supply,
                mint_to,
            );
            // Alice approves Bob for token transfers on her behalf.
            let alice_balance = gtx.balance_of(accounts.alice);
            let initial_allowance = alice_balance + 2;
            assert_eq!(gtx.approve(accounts.bob, initial_allowance), Ok(()));

            // Get contract address.
            let callee = ink::env::account_id::<ink::env::DefaultEnvironment>();
            ink::env::test::set_callee::<ink::env::DefaultEnvironment>(callee);
            ink::env::test::set_caller::<ink::env::DefaultEnvironment>(accounts.bob);

            // Bob tries to transfer tokens from Alice to Eve.
            let emitted_events_before = ink::env::test::recorded_events().count();
            assert_eq!(
                gtx.transfer_from(accounts.alice, accounts.eve, alice_balance + 1),
                Err(Error::InsufficientBalance)
            );
            // Allowance must have stayed the same
            assert_eq!(
                gtx.allowance(accounts.alice, accounts.bob),
                initial_allowance
            );
            // No more events must have been emitted
            assert_eq!(
                emitted_events_before,
                ink::env::test::recorded_events().count()
            )
        }

        /// For calculating the event topic hash.
        struct PrefixedValue<'a, 'b, T> {
            pub prefix: &'a [u8],
            pub value: &'b T,
        }

        impl<X> scale::Encode for PrefixedValue<'_, '_, X>
        where
            X: scale::Encode,
        {
            #[inline]
            fn size_hint(&self) -> usize {
                self.prefix.size_hint() + self.value.size_hint()
            }

            #[inline]
            fn encode_to<T: scale::Output + ?Sized>(&self, dest: &mut T) {
                self.prefix.encode_to(dest);
                self.value.encode_to(dest);
            }
        }

        fn encoded_into_hash<T>(entity: &T) -> Hash
        where
            T: scale::Encode,
        {
            use ink::{
                env::hash::{
                    Blake2x256,
                    CryptoHash,
                    HashOutput,
                },
                primitives::Clear,
            };

            let mut result = Hash::CLEAR_HASH;
            let len_result = result.as_ref().len();
            let encoded = entity.encode();
            let len_encoded = encoded.len();
            if len_encoded <= len_result {
                result.as_mut()[..len_encoded].copy_from_slice(&encoded);
                return result
            }
            let mut hash_output =
                <<Blake2x256 as HashOutput>::Type as Default>::default();
            <Blake2x256 as CryptoHash>::hash(&encoded, &mut hash_output);
            let copy_len = core::cmp::min(hash_output.len(), len_result);
            result.as_mut()[0..copy_len].copy_from_slice(&hash_output[0..copy_len]);
            result
        }
    }

    #[cfg(all(test, feature = "e2e-tests"))]
    mod e2e_tests {
        use super::*;
        use ink_e2e::build_message;
        type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

        #[ink_e2e::test]
        async fn e2e_transfer(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
            // given
            let total_supply = 1_000_000_000;
            let constructor = Erc20Ref::new(total_supply);
            let contract_acc_id = client
                .instantiate("gtx", &ink_e2e::alice(), constructor, 0, None)
                .await
                .expect("instantiate failed")
                .account_id;

            // when
            let total_supply_msg = build_message::<Erc20Ref>(contract_acc_id.clone())
                .call(|gtx| gtx.total_supply());
            let total_supply_res = client
                .call_dry_run(&ink_e2e::bob(), &total_supply_msg, 0, None)
                .await;

            let bob_account = ink_e2e::account_id(ink_e2e::AccountKeyring::Bob);
            let transfer_to_bob = 500_000_000u128;
            let transfer = build_message::<Erc20Ref>(contract_acc_id.clone())
                .call(|gtx| gtx.transfer(bob_account.clone(), transfer_to_bob));
            let _transfer_res = client
                .call(&ink_e2e::alice(), transfer, 0, None)
                .await
                .expect("transfer failed");

            let balance_of = build_message::<Erc20Ref>(contract_acc_id.clone())
                .call(|gtx| gtx.balance_of(bob_account));
            let balance_of_res = client
                .call_dry_run(&ink_e2e::alice(), &balance_of, 0, None)
                .await;

            // then
            assert_eq!(
                total_supply,
                total_supply_res.return_value(),
                "total_supply"
            );
            assert_eq!(transfer_to_bob, balance_of_res.return_value(), "balance_of");

            Ok(())
        }

        #[ink_e2e::test]
        async fn e2e_allowances(mut client: ink_e2e::Client<C, E>) -> E2EResult<()> {
            // given
            let total_supply = 1_000_000_000;
            let constructor = Erc20Ref::new(total_supply);
            let contract_acc_id = client
                .instantiate("gtx", &ink_e2e::bob(), constructor, 0, None)
                .await
                .expect("instantiate failed")
                .account_id;

            // when

            let bob_account = ink_e2e::account_id(ink_e2e::AccountKeyring::Bob);
            let charlie_account = ink_e2e::account_id(ink_e2e::AccountKeyring::Charlie);

            let amount = 500_000_000u128;
            let transfer_from =
                build_message::<Erc20Ref>(contract_acc_id.clone()).call(|gtx| {
                    gtx.transfer_from(
                        bob_account.clone(),
                        charlie_account.clone(),
                        amount,
                    )
                });
            let transfer_from_result = client
                .call(&ink_e2e::charlie(), transfer_from, 0, None)
                .await;

            assert!(
                transfer_from_result.is_err(),
                "unapproved transfer_from should fail"
            );

            // Bob approves Charlie to transfer up to amount on his behalf
            let approved_value = 1_000u128;
            let approve_call = build_message::<Erc20Ref>(contract_acc_id.clone())
                .call(|gtx| gtx.approve(charlie_account.clone(), approved_value));
            client
                .call(&ink_e2e::bob(), approve_call, 0, None)
                .await
                .expect("approve failed");

            // `transfer_from` the approved amount
            let transfer_from =
                build_message::<Erc20Ref>(contract_acc_id.clone()).call(|gtx| {
                    gtx.transfer_from(
                        bob_account.clone(),
                        charlie_account.clone(),
                        approved_value,
                    )
                });
            let transfer_from_result = client
                .call(&ink_e2e::charlie(), transfer_from, 0, None)
                .await;
            assert!(
                transfer_from_result.is_ok(),
                "approved transfer_from should succeed"
            );

            let balance_of = build_message::<Erc20Ref>(contract_acc_id.clone())
                .call(|gtx| gtx.balance_of(bob_account));
            let balance_of_res = client
                .call_dry_run(&ink_e2e::alice(), &balance_of, 0, None)
                .await;

            // `transfer_from` again, this time exceeding the approved amount
            let transfer_from =
                build_message::<Erc20Ref>(contract_acc_id.clone()).call(|gtx| {
                    gtx.transfer_from(bob_account.clone(), charlie_account.clone(), 1)
                });
            let transfer_from_result = client
                .call(&ink_e2e::charlie(), transfer_from, 0, None)
                .await;
            assert!(
                transfer_from_result.is_err(),
                "transfer_from exceeding the approved amount should fail"
            );

            assert_eq!(
                total_supply - approved_value,
                balance_of_res.return_value(),
                "balance_of"
            );

            Ok(())
        }
    }
}