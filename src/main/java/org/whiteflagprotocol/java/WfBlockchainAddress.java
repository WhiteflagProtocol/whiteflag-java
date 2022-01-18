/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

/**
 * Whiteflag blockchain address interface
 * 
 * <p> This interface defines how a blockchain address must be represented
 * to WFJL classes that need a blockchain address. Whiteflag is blockchain
 * agnostic, but (the representation of) adresses are blockchain specific.
 * This interface is to be used by implementation specific classes that
 * provide Whiteflag classes with a blockchain address, such as classes
 * representing a blockchain, a blockchain account, or a blockchain address.
 * 
 * @since 1.1
 */
public interface WfBlockchainAddress {

    /* PUBLIC METHODS */

    /**
     * Returns the blockchain address as a string in the encoding specified for that specific blockchain
     * 
     * <p> Blockchain addresses are encoded differently for different
     * blockchains. For example, Bitcoin uses a modified Base58 encoding
     * and Ethereum uses a hexadecimal string.
     * 
     * @return a string with the blockchain address in the encoding used for that blockchain
     */
    public String getAddressString();

    /**
     * Provides the binary blockchain address indepent from the representation for that blockchain
     * 
     * <p> Although blockchain addresses are encodedd differently, they all
     * are essentially a binary value. Whiteflag uses that binary value to
     * bind cryptographic keys and tokens to an address.
     * 
     * @return a byte array with the binary blockchain address
     */
    public byte[] getBinaryAddress();
}
