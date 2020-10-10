/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag message segment class
 * 
 * </p> This is a class representing a segment of a Whiteflag message, such as a
 * the message header or the message body. A message segment contains a number of
 * message fields, depending on the part and the type of the message. The fields
 * should be ordered without missing or overlapping bytes.
 */
public class WfMessageSegment {

    /* PROPERTIES */

    /* Array of message segment fields */
    private WfMessageField[] fields;

    /* Deserialisation and Decoding cursor */
    private int cursor = 0;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag message segment from a {@link WfMessageField} array, without copying field values
     * @param fields an array of {@link WfMessageField}s
     */
    public WfMessageSegment(final WfMessageField[] fields) {
        this.fields = new WfMessageField[fields.length];

        // Deep copy of the fields, using the copy constructor (which does not copy the field values)
        for (int i=0; i < fields.length; i++) {
            this.fields[i] = new WfMessageField(fields[i]);
        }
    }

    /**
     * Constructs a new Whiteflag message segment from another message segment, cloning its fields including values
     * @param segment the {@link WfMessageSegment} to create the new segment from
     */
    public WfMessageSegment(final WfMessageSegment segment) {
        this.fields = new WfMessageField[segment.getNoFields()];
        this.fields = segment.getFields().clone();
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the message segment as a concatinated string of field values
     * @return String with serialized message segment
     */
    @Override
    public final String toString() {
        String string;
        try {
            string = this.serialize();
        } catch (WfCoreException e) {
            return "";
        }
        return string; 
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Checks if all fields of this message segment contain valid data
     * @return TRUE if message segment contains valid data, esle FALSE
     */
    public final Boolean isValid() {
        int currentByte = fields[0].startByte;
        for (WfMessageField field : fields) {
            // Fields should be ordered without missing or overlapping bytes
            if (field.startByte != currentByte) return false;
            currentByte = field.endByte;
            // Field should be valid
            if (Boolean.TRUE.equals(!field.isValid())) return false;
        }
        return true;
    }

    /* PUBLIC METHODS: object operations */

    /**
     * Serializes this message segment
     * @return a string with the serialized message segment
     * @throws WfCoreException if the message cannot be serialized
     */
    public final String serialize() throws WfCoreException {
        int currentByte = fields[0].startByte;
        StringBuilder s = new StringBuilder();

        for (WfMessageField field : fields) {
            if (field.startByte != currentByte) {
                throw new WfCoreException("Invalid field order while serializing: did not expect field " + field.name + " at byte " + currentByte);
            }
            s.append(field.getValue());
            currentByte = field.endByte;
        }
        return s.toString();
    }

        /**
     * Deserializes this message segment from the provided serialized message
     * @param messageStr String with the serialized message
     * @param byteCursor the byte position where this segment starts in the serialized message
     * @return the byte position where this segment ends in the serialized message
     */
    public final int deserialize(final String messageStr, int byteCursor) throws WfCoreException {
        for (; cursor < this.fields.length; cursor++) {
            String value;

            // Get field value from serialized message part
            if (fields[cursor].endByte < 0) {
                value = messageStr.substring(fields[cursor].startByte);
            } else {
                value = messageStr.substring(fields[cursor].startByte, fields[cursor].endByte);
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(fields[cursor].setValue(value))) {
                throw new WfCoreException(fields[cursor].debugString() + " already set or invalid data in serialized message at byte " + byteCursor + ": " + value);
            }
            // Move to next field in serialized message
            byteCursor = fields[cursor].endByte;
        }
        return byteCursor;
    }

    /**
     * Encodes this message segment
     * @return a string with the serialized message segment
     * @throws WfCoreException if the message cannot be encoded
     */
    public final WfBinaryString encode() throws WfCoreException {
        int currentByte = fields[0].startByte;
        WfBinaryString bin = new WfBinaryString();
        
        for (WfMessageField field : fields) {
            if (field.startByte != currentByte) {
                throw new WfCoreException("Invalid field order while encoding: did not expect field " + field.name + " at byte " + currentByte);
            }
            bin.append(field.encode());
            currentByte = field.endByte;
        }
        return bin;
    }

    /**
     * Decodes this message segment from the provided encoded message
     * @param messageBinStr {@link WfBinaryString} with the encoded message
     * @param bitCursor the bit position where this segment starts in the encoded message
     * @return the bit position where this segment ends in the encoded message
     */
    public final int decode(final WfBinaryString messageBinStr, int bitCursor) throws WfCoreException {
        for (; cursor < this.fields.length; cursor++) {
            final int endBit = bitCursor + fields[cursor].bitLength();
            String value;
            
            // Decode the field from the encoded message part
            if (fields[cursor].endByte < 0) {
                value = fields[cursor].decode(messageBinStr.sub(bitCursor));
            } else {
                value = fields[cursor].decode(messageBinStr.sub(bitCursor, endBit));
            }
            // Set the field value
            if (Boolean.FALSE.equals(fields[cursor].setValue(value))) {
                throw new WfCoreException(fields[cursor].debugString() + " already set or invalid data in encoded binary message at bit " + bitCursor + ": " + value);
            }
            // Move to next field in encoded message
            bitCursor = endBit;
        }
        return bitCursor;
    }

    /* PUBLIC METHODS: field operations */

    /**
     * Gets the number of fields in this message segment
     * @return integer with the numbver of fields
     */
    public final int getNoFields() {
        return this.fields.length;
    }

    /**
     * Gets the value of the field specified by name
     * @param name String with the name of the requested field
     * @return String with the field value, or NULL if field does not exist
     */
    public final String getFieldValue(final String name) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field.getValue();
        }
        return null;
    }

    /**
     * Gets the value of the field specified by index
     * @param index integer with the index of the requested field
     * @return String with the field value, or NULL if it does not exist
     */
    public final String getFieldValue(int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index].getValue();
        }
        return null;
    }

    /**
     * Sets the value of the specified field in the message segment
     * @param name String with the name of the field
     * @param data String with data to be set as the field value
     * @return TRUE if field value is set, FALSE if field does not exits, isalready set, or data is invalid
     */
    public final Boolean setFieldValue(final String name, final String data) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field.setValue(data);
        }
        return false;
    }

    /**
     * Sets the value of the field specified by its index in the message segment
     * @param index Integer with the name of the field
     * @param data String with data to be set as the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public final Boolean setFieldValue(final int index, final String data) {
        if (index >= 0 && index < fields.length) {
            return fields[index].setValue(data);
        }
        return false;
    }

    /* PROTECTED METHODS: object operations */

    /**
     * Appends additional fields to this message segment if constructing complex message bodies
     * @param segment {@link WfMessageSegment} to be added to the message segment
     * @return The updated message segment object
     */
    protected final WfMessageSegment append(final WfMessageSegment segment) {
        // Create new field array and fill with original fields from this segment
        WfMessageField[] newFieldArray = new WfMessageField[this.fields.length + segment.getNoFields()];
        System.arraycopy(this.fields, 0, newFieldArray, 0, this.fields.length);

        // Get last byte of original field array to shit bytes in added array
        int shift = this.fields[this.fields.length].endByte;

        // Add new fields from other segment with shifted start and end byte to array
        int i = this.fields.length;
        for (WfMessageField field : segment.getFields()) {
            newFieldArray[i] = new WfMessageField(field, shift);
            i++;
        }
        //Set the fields with new field array and return this object
        this.fields = newFieldArray;
        return this;
    }

    /* PROTECTED METHODS: field operations */

    /**
     * Gets the field specified by name
     * @param name String with the name of the requested field
     * @return the requested {@link WfMessageField}, or NULL if it does not exist
     */
    protected final WfMessageField getField(final String name) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field;
        }
        return null;
    }

    /**
     * Gets the field specified by index
     * @param index integer with the index of the requested field
     * @return the requested {@link WfMessageField}, or NULL if it does not exist
     */
    protected final WfMessageField getField(final int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index];
        }
        return null;
    }

    /**
     * Gets all fields from this message segment
     * @return Array of {@link WfMessageField}
     */
    protected final WfMessageField[] getFields() {
        return this.fields;
    }

    /**
     * Sets the value of all fields in the message segment with values from an array
     * @param data Array of strings with with data to be set as the field values
     * @return TRUE if the data was valid and all field values are set
     * @throws WfCoreException if the provided data is invalid
     */
    protected final Boolean setAllFieldValues(final String[] data) throws WfCoreException {
        if (data.length != fields.length) {
            throw new WfCoreException("Message part has " + fields.length + " fields, but have data for " + data.length + " fields");
        }
        for (int n = 0; n < fields.length; n++) {
            if (Boolean.FALSE.equals(fields[n].setValue(data[n]))) {
                throw new WfCoreException(fields[n].debugString() + " already set or invalid data provided: " + data[n]);
            }
        }
        return true;
    }
}
