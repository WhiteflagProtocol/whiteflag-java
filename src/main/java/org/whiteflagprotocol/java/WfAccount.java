package org.whiteflagprotocol.java;

import java.net.URL;
import java.security.interfaces.ECPublicKey;

import org.whiteflagprotocol.java.crypto.WfAuthToken;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;

/**
 * Whiteflag account interface
 * 
 * <p> This interface defines how a class representing a blockchain account
 * used for Whiteflag interacts with WFJL classes. Blockchains do not have a
 * central user account database, and even the concept of an account does not
 * exist on some blockchains. Therefore, this Whiteflag account is merely an
 * abstraction for information related to a Whiteflag participant, including
 * oneself.
 * 
 * <p> Because different blockchain use the concept of an account differently,
 * the actual implementation is blockchain-specific. This interface therefore
 * defines how an object representing a blockchain account of a Whiteflag
 * participiant must provide relevant information to instances of WFJL classes
 * such a {@link WfMessage}. Information about a Whiteflag participant includes
 * the blockchain address, and encryption keys and authentication tokens used
 * with that address. Methods implementing this interface may return `null` if
 * certain data is unknown or does not exist for a particpant.
 *  
 * @since 1.1
 */
public interface WfAccount {

    /**
     * Checks if this is an own account
     * @return TRUE if own account, else FALSE if other's account
     */
    public boolean isSelf();

    /**
     * Returns the blockchain address as a string in the encoding specified for that specific blockchain
     * 
     * <p> Blockchain addresses are encoded differently for different
     * blockchains. For example, Bitcoin uses a modified Base58 encoding
     * and Ethereum uses a hexadecimal string.
     * 
     * @return a string with the blockchain address in the encoding used for that blockchain, or null if no known address
     */
    public String getAddress();

    /**
     * Provides the binary blockchain address indepent from the representation for that blockchain
     * 
     * <p> Although addresses of diffrent blockchains are encoded differently,
     * they all are essentially a binary value. Whiteflag uses that binary
     * value to bind cryptographic keys and tokens to an address.
     * 
     * @return a byte array with the binary blockchain address
     */
    public byte[] getBinaryAddress();

    /**
     * Gets the authentication URL sent with an A1 message used to identify the originator associated with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a URL pointing to the authentication data, or null if no known url
     */
    public URL getAuthURL();

    /**
     * Gets the authentication token sent with an A2 message to identify the originator associated with this account
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the authentication token, or null if no knwon token
     */
    public WfAuthToken getAuthToken();

    /**
     * Gets the shared encryption key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared encryption key, or null if no key available
     */
    public WfEncryptionKey getSharedKey();


    /**
     * Gets the own ECDH key pair used to negatiate keys with other participants
     * @wfref 5.2.4 Message Encryption
     * @return the ECDH public key, or null if no key pair available
     */
    public WfECDHKeyPair getEcdhKeyPair();

    /**
     * Gets the other's ECDH public key used to negatioate a key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the ECDH public key, or null is no public key available
     */
    public ECPublicKey getEcdhPublicKey();    
}
